package simulation

import common.Dataflow
import scala.collection.mutable

//Remember modeling not only modeling the systolic tensor array itself, modeling pre- and post-processor too
final class Array(
  val arrayConfig: ArrayConfig,
  val loggerOption: LoggerOption,
)extends Hardware with Logger with AccessCounter{

  setMode(loggerOption)

  private var isArrayOutputStall = false
  var arrayInputStallCount = 0
  var arrayOutputStallCount = 0

  var tileSizeA: Int = 0
  var tileSizeB: Int = 0

  private var tileIdToReceiveA: (Int,Int) = (-1, -1)
  private var tileIdToReceiveB: (Int,Int) = (-1, -1)

  private var schedule: Vector[ScheduledOperation] = _

  private val currentTileQueueTypeA: mutable.Queue[TileA] = mutable.Queue.empty[TileA]
  private val nextTileQueueTypeA: mutable.Queue[TileA] = mutable.Queue.empty[TileA]

  private val currentTileQueueTypeB: mutable.Queue[TileB] = mutable.Queue.empty[TileB]
  private val nextTileQueueTypeB: mutable.Queue[TileB] = mutable.Queue.empty[TileB]

  private val calculatingOperation: mutable.Queue[ScheduledOperation] = mutable.Queue.empty[ScheduledOperation]

  //TODO change Memory to SRAM
  private var totalMemoryAccessCountA: Double = 0
  private var totalMemoryHitCountA: Double = 0
  private var totalMemoryMissCountA: Double = 0

  private var totalMemoryAccessCountB: Double = 0
  private var totalMemoryHitCountB: Double = 0
  private var totalMemoryMissCountB: Double = 0

  private var arrayActiveCount: Int = 0

  //Functions called by compiler
  def getMemoryAccessCountA: Double = totalMemoryAccessCountA
  def getMemoryHitCountA: Double = totalMemoryHitCountA
  def getMemoryMissCountA: Double = totalMemoryMissCountA

  def getMemoryAccessCountB: Double = totalMemoryAccessCountB
  def getMemoryHitCountB: Double = totalMemoryHitCountB
  def getMemoryMissCountB: Double = totalMemoryMissCountB

  def isAllCalculated: Boolean = schedule.forall(_.isCalculated)
  def getArrayActiveCount: Int = arrayActiveCount

  def initTileSize(multiplicationOperation: MultiplicationOperation): Unit = {

    tileSizeA = multiplicationOperation.generateTileA.dims.memorySize
    tileSizeB = multiplicationOperation.generateTileB.dims.memorySize

  }

  def uploadOperationVector(operationVector: Vector[MultiplicationOperation]): Unit = {

    def createScheduledOperation(
      op: MultiplicationOperation,
      isTileAUsedInNext: Boolean = false,
      isTileBUsedInNext: Boolean = false
    ) = {
      ScheduledOperation(
        arrayConfig.dataflow,
        op.layerName,
        op.operationId,
        op.generateTileC,
        isTileAUsedInNextOp = isTileAUsedInNext,
        isTileBUsedInNextOp = isTileBUsedInNext,
        loggerOption
      )
    }

    def processWithSliding(checkNextTile: (MultiplicationOperation, MultiplicationOperation) => Boolean) = {
      if (operationVector.isEmpty){
        Console.err.println("There is nothing to upload in operation vector")
        sys.exit(1)

      } else if(operationVector.size == 1) {
        Vector(createScheduledOperation(operationVector.head))
      } else {
        val initialSchedule =  operationVector.sliding(2).toVector.collect{
          case Seq(curr, next) =>
            val isUsedInNext = checkNextTile(curr, next)
            createScheduledOperation(
              curr,
              isTileAUsedInNext = arrayConfig.dataflow == Dataflow.Is && isUsedInNext,
              isTileBUsedInNext = arrayConfig.dataflow == Dataflow.Ws && isUsedInNext
            )
        }
        initialSchedule :+ createScheduledOperation(operationVector.last)
      }
    }

    schedule = arrayConfig.dataflow match {
      case Dataflow.Is =>
        processWithSliding((curr, next) => curr.getTileAId == next.getTileAId)

      case Dataflow.Os =>
        operationVector.map(createScheduledOperation(_))

      case Dataflow.Ws =>
        processWithSliding((curr, next) => curr.getTileBId == next.getTileBId)

      case _ =>
        Console.err.println(s"[error] Invalid dataflow")
        sys.exit(1)
    }

  }

  //Function called by Input double buffer SRAM
  def receive(tile: Tile): Unit = {

    assert(tile.memoryOccupiedByArray > 0,
      s"[error] Array cannot take this tile ${tile.id}")
    assert(tile.dataType == DataType.A || tile.dataType == DataType.B,
      s"[error] Array cannot receive this type of tile")

    tile.dataType match {
      case DataType.A =>
        nextTileQueueTypeA.enqueue(tile.asInstanceOf[TileA])
      case DataType.B =>
        nextTileQueueTypeB.enqueue(tile.asInstanceOf[TileB])
      case _ =>
        Console.err.println(s"[error] Array receive wrong data tiles ${tile.dataType}")
        sys.exit(1)
    }

    incrementWriteAccessCount()

  }

  //Functions called by Output Double buffer SRAM
  def stop(): Unit = {
    isArrayOutputStall = true
  }

  def go(): Unit = {
    isArrayOutputStall = false
  }

  override def update(interface: Interface) : Unit = {

    //TODO consider output double buffer SRAM capacity
    if (calculatingOperation.nonEmpty && !isArrayOutputStall) {
      prepareTileForSend()
      send(interface)
    } else
      stuck = true

    countSramAccess(interface)
    updateState()

    changeOperationState(interface)

    resetTileIdToReceive()
    setTileIdToReceive()

    if(!isArrayOutputStall) {

      calculatingOperation.foreach(op => op.updateTimer())
      countArrayActiveState()
      judgeSramDoubleBufferLogic(interface)

      arrayConfig.dataflow match {
        case Dataflow.Is | Dataflow.Ws => setSendingTileInSramIsOrWs(interface)
        case Dataflow.Os => setSendingTileInSramOs(interface)
        case _ =>
          Console.err.println(s"[error] Invalid dataflow")
          sys.exit(1)
      }
    } else {
      arrayOutputStallCount += 1
    }

    if(isAllCalculated){
      interface.sramA.clearBuffer()
      interface.sramB.clearBuffer()
    }

  }

  private def countArrayActiveState(): Unit = {
    if(calculatingOperation.nonEmpty)
      arrayActiveCount += 1
  }

  //TODO change code below see double buffer sram countDramAccess
  private def countSramAccess(interface: Interface): Unit = {

    if(tileIdToReceiveA != (-1, -1) && interface.sramA.isFirstFillUpCompete) {
      totalMemoryAccessCountA += tileSizeA
      if(interface.sramA.hasThisTileForArray(tileIdToReceiveA, DataType.A) &&
        capacityLeftTileA() >= arrayConfig.bandwidthOfInputA &&
        nextTileQueueTypeA.exists(_.id == tileIdToReceiveA)){
        totalMemoryHitCountA += tileSizeA
      } else {
        arrayInputStallCount += 1
        totalMemoryMissCountA += tileSizeA
      }
    }

    if(tileIdToReceiveB != (-1, -1) && interface.sramB.isFirstFillUpCompete) {
      totalMemoryAccessCountB += tileSizeB
      if(interface.sramB.hasThisTileForArray(tileIdToReceiveB, DataType.B) &&
        capacityLeftTileB() >= arrayConfig.bandwidthOfInputB &&
        nextTileQueueTypeB.exists(_.id == tileIdToReceiveB)){
        totalMemoryHitCountB += tileSizeB
      } else {
        arrayInputStallCount += 1
        totalMemoryMissCountB += tileSizeB
      }
    }

  }

  private def judgeSramDoubleBufferLogic(interface: Interface): Unit = {

    if(tileIdToReceiveA != (-1, -1))
      if(!interface.sramA.hasThisTileForArray(tileIdToReceiveA, DataType.A))
        interface.sramA.judgeDoubleBufferState()

    if(tileIdToReceiveB != (-1, -1))
      if(!interface.sramB.hasThisTileForArray(tileIdToReceiveB, DataType.B))
        interface.sramB.judgeDoubleBufferState()

  }

  private def setSendingTileInSramIsOrWs(interface: Interface): Unit = {

    if(interface.sramA.hasThisTileForArray(tileIdToReceiveA, DataType.A) && tileIdToReceiveA != (-1, -1))
      if(capacityLeftTileA() >= arrayConfig.bandwidthOfInputA)
        interface.sramA.setTileIdToSend(tileIdToReceiveA)

    if(interface.sramB.hasThisTileForArray(tileIdToReceiveB, DataType.B) && tileIdToReceiveB != (-1, -1))
      if(capacityLeftTileB() >= arrayConfig.bandwidthOfInputB)
        interface.sramB.setTileIdToSend(tileIdToReceiveB)

  }

  private def setSendingTileInSramOs(interface: Interface): Unit = {

    if(
      interface.sramA.hasThisTileForArray(tileIdToReceiveA, DataType.A)
        && interface.sramB.hasThisTileForArray(tileIdToReceiveB, DataType.B)
        && tileIdToReceiveA != (-1, -1)
        && tileIdToReceiveB != (-1, -1)
    ){
      if(capacityLeftTileA() >= arrayConfig.bandwidthOfInputA)
        interface.sramA.setTileIdToSend(tileIdToReceiveA)

      if(capacityLeftTileB() >= arrayConfig.bandwidthOfInputB)
        interface.sramB.setTileIdToSend(tileIdToReceiveB)

    }

  }

  override def prepareTileForSend(): Unit = {

    val op = calculatingOperation.front
    val tileA = op.getTileA
    val tileB = op.getTileB
    val tileC = op.getTileC

    if(op.isInputATileGoneTimerExpired(arrayConfig.dataflow) && !op.isTileAUsedInNextOp)
      coloringTileAorB(tileA, arrayConfig.bandwidthOfInputA)

    if(op.isInputBTileGoneTimerExpired(arrayConfig.dataflow) && !op.isTileBUsedInNextOp)
      coloringTileAorB(tileB, arrayConfig.bandwidthOfInputB)

    if(op.isOutputTileGenerationTimerExpired)
      coloringTileC(tileC)

  }

  private def coloringTileAorB(targetTile: Tile, bandwidth: Int): Unit = {
    if(targetTile.memoryOccupiedByArray - bandwidth >= 0 ){
      targetTile.memoryCalculatedByArray = targetTile.memoryCalculatedByArray + bandwidth
      targetTile.memoryOccupiedByArray = targetTile.memoryOccupiedByArray - bandwidth
    } else {
      targetTile.memoryCalculatedByArray = targetTile.memoryCalculatedByArray + targetTile.memoryOccupiedByArray
      targetTile.memoryOccupiedByArray = 0
    }
  }

  private def coloringTileC(targetTileC: Tile): Unit = {
    if(arrayConfig.outputBandwidth - targetTileC.memoryOccupiedByArray >= 0){
      targetTileC.memoryOccupiedBySram = targetTileC.dims.memorySize
      targetTileC.memoryOccupiedByArray = 0
    } else {
      targetTileC.memoryOccupiedBySram += arrayConfig.outputBandwidth
      targetTileC.memoryOccupiedByArray = targetTileC.dims.memorySize - targetTileC.memoryOccupiedBySram
    }
  }

  override def send(interface: Interface) : Unit = {
    if(calculatingOperation.isEmpty){
      println("SSibal")
      return
    }
    val operation = calculatingOperation.front
    val targetTileA = operation.getTileA
    val targetTileB = operation.getTileB
    val targetTileC = operation.getTileC

    require(operation.isCalculating, s"[error] Only calculating operation can enter this function" +
      s" ID: ${operation.operationId}")

    if(targetTileC.memoryOccupiedBySram > 0 ){

      if(targetTileC.ownedBySram) {

        assert(targetTileA.ownedByArray, s"[error] Array dose not have this tile completely" +
          s" Operation ID: ${operation.operationId} Tile A Id: ${operation.getTileAId}" +
          s"DRAM: ${operation.getTileA.memoryOccupiedByDram} SRAM: ${operation.getTileA.memoryOccupiedBySram}" +
          s" Array: ${operation.getTileA.memoryOccupiedByArray}" +
          s" Calculated: ${operation.getTileA.memoryCalculatedByArray}" +
          s" tile A col"

        )

        assert(targetTileB.ownedByArray, s"[error] Array dose not have this tile completely" +
          s" Operation ID: ${operation.operationId} Tile B Id: ${targetTileB.printTile()}" +
          s" Dataflow: ${arrayConfig.dataflow}"
        )

        operation.completeCalculation()

        val tileAIndex: Int = currentTileQueueTypeA.indexWhere(tile => tile.id == targetTileA.id)
        val tileBIndex: Int = currentTileQueueTypeB.indexWhere(tile => tile.id == targetTileB.id)

        assert(tileAIndex != -1, s"[error] Cannot find tile in array ID: ${operation.operationId}")
        assert(tileBIndex != -1, s"[error] Cannot find tile in array ID: ${operation.operationId}")

        if(!operation.isTileAUsedInNextOp)
          currentTileQueueTypeA.remove(tileAIndex)
        else
          currentTileQueueTypeA(tileAIndex).completeLoading()

        if(!operation.isTileBUsedInNextOp)
          currentTileQueueTypeB.remove(tileBIndex)
        else
          currentTileQueueTypeB(tileBIndex).completeLoading()

        interface.sramC.receive(targetTileC.copyTile())
        calculatingOperation.removeHead()

      } else
        interface.sramC.receive(targetTileC.copyTile())

      incrementReadAccessCount()
    }


  }


  override def updateState(): Unit = {

    while(nextTileQueueTypeA.nonEmpty){
      if(currentTileQueueTypeA.isEmpty){
        currentTileQueueTypeA.enqueue(nextTileQueueTypeA.dequeue())
      } else {
        if(currentTileQueueTypeA.last.id == nextTileQueueTypeA.head.id && !currentTileQueueTypeA.last.ownedByArray){
          currentTileQueueTypeA.last.copyDataTransferData(nextTileQueueTypeA.head)
          nextTileQueueTypeA.dequeue()
        } else {
          currentTileQueueTypeA.enqueue(nextTileQueueTypeA.dequeue())
        }
      }
    }

    while(nextTileQueueTypeB.nonEmpty){
      if(currentTileQueueTypeB.isEmpty){
        currentTileQueueTypeB.enqueue(nextTileQueueTypeB.dequeue())
      } else {
        if(currentTileQueueTypeB.last.id == nextTileQueueTypeB.head.id && !currentTileQueueTypeB.last.ownedByArray){
          currentTileQueueTypeB.last.copyDataTransferData(nextTileQueueTypeB.head)
          nextTileQueueTypeB.dequeue()
        } else {
          currentTileQueueTypeB.enqueue(nextTileQueueTypeB.dequeue())
        }
      }
    }

  }

  private def changeOperationState(interface: Interface): Unit =
    arrayConfig.dataflow match {
      case Dataflow.Is =>
        changeOperationStateIs(interface)
      case Dataflow.Os =>
        changeOperationStateOs(interface)
      case Dataflow.Ws =>
        changeOperationStateWs(interface)
    }

  private def changeOperationStateIs(interface: Interface): Unit = {
    assert(arrayConfig.dataflow == Dataflow.Is, "[error] Wrong dataflow need Is for this function")

    val op = schedule.find(op => op.isNextCalculation || op.isLoading || op.isLoaded || op.isWaiting)

    op match {
      case Some(op) if op.isNextCalculation =>

        if (currentTileQueueTypeA.exists(tile => tile.id == op.getTileAId && tile.isWaiting)) {

//          op.setTileA(currentTileQueueTypeA.find(tile => tile.id == op.getTileAId && tile.isWaiting).get)
//          interface.sramA.changeTileStateIfExists(op.getTileAId, TileState.loading)
//          op.startLoading(arrayConfig.dataflow)
//
//          if (op.getTileA.ownedByArray)
//            op.completeLoading(arrayConfig.dataflow)

          var opIndex = schedule.indexOf(op)
          val loadingTileA = currentTileQueueTypeA.find(tile => tile.id == op.getTileAId && tile.isWaiting).get
          interface.sramA.changeTileStateIfExists(loadingTileA.id, TileState.loading)

//          val loadingOutputTileIDs: mutable.Queue[(Int, Int, Int)] = interface.sramB.getReadBufferIDs.map { id =>
//            assert(loadingTileA.id._2 == id._1, "[error] ID does not match")
//            (loadingTileA.id._1, id._2, id._1)
//          }

          val loadingOutputTileIDs: mutable.Queue[(Int, Int, Int)] = interface.sramB.getReadBufferIDs
            .filter(_._1 == loadingTileA.id._2 )
            .map(id =>(loadingTileA.id._1, id._2, id._1))

          while(loadingOutputTileIDs.nonEmpty){

            if((schedule(opIndex).isNextCalculation || schedule(opIndex).isWaiting) &&
              schedule(opIndex).operationId == loadingOutputTileIDs.head){

              schedule(opIndex).setTileA(loadingTileA)
              schedule(opIndex).startLoading(arrayConfig.dataflow)
              opIndex += 1
              loadingOutputTileIDs.dequeue()

            }

          }
        }

      case Some(op) if op.isLoading =>

        if (op.getTileA.ownedByArray) {

//          op.completeLoading(arrayConfig.dataflow)
//          val opIndex = schedule.indexOf(op)
//          var nextIndex = opIndex + 1
//
//          while(
//            nextIndex < schedule.length &&
//              schedule(nextIndex).isWaiting &&
//              schedule(nextIndex).getTileAId == op.getTileAId
//          ){
//            schedule(nextIndex).setTileA(op.getTileA)
//            schedule(nextIndex).completeLoading(arrayConfig.dataflow)
//            nextIndex += 1
//          }

          val loadedTile = op.getTileA
          val loadingOperations = schedule.filter( op => op.isLoading && loadedTile.id == op.getTileAId)

          loadingOperations.foreach{ op =>
            if(interface.sramB.getReadBufferIDs.exists(_._1 == loadedTile.id._2))
              op.completeLoading(arrayConfig.dataflow)
          }


        }

//  loaded previous code
//      case Some(op) if op.isLoaded =>
//        if (currentTileQueueTypeB.exists(tile => tile.id == op.getTileBId && tile.isWaiting && tile.memoryOccupiedByArray > 0)) {
//          op.setTileB(currentTileQueueTypeB.find(tile => tile.id == op.getTileBId && tile.isWaiting && tile.memoryOccupiedByArray > 0).get)
//          interface.sramB.changeTileStateIfExists(op.getTileBId, TileState.calculating)
//          op.startCalculation(arrayConfig)
//          calculatingOperation += op
//        }

      case Some(op) if op.isLoaded =>

        if (currentTileQueueTypeB.exists(
          tile => tile.id == op.getTileBId && tile.isWaiting && tile.memoryOccupiedByArray > 0)
        ){
          op.setTileB( currentTileQueueTypeB.find(
            tile => tile.id == op.getTileBId && tile.isWaiting && tile.memoryOccupiedByArray > 0
          ).get)
          interface.sramB.changeTileStateIfExists(op.getTileBId, TileState.calculating)
          op.startCalculation(arrayConfig)
          calculatingOperation += op
        }


      case Some(op) if op.isWaiting =>

        if (capacityLeftTileA() >= arrayConfig.bandwidthOfInputA)
          schedule.find(op => op.isWaiting).get.willCalculateTile()

      case _ =>
      //Do nothing

    }


  }

  private def changeOperationStateOs(interface: Interface): Unit = {
    require(arrayConfig.dataflow == Dataflow.Os, "[error] Wrong dataflow need Os for this function")

    val op = schedule.find(op => op.isNextCalculation || op.isWaiting)

    op match {
      case Some(op) if op.isNextCalculation =>
        if(currentTileQueueTypeA.exists(tile => tile.id == op.getTileAId && tile.isWaiting )
          && currentTileQueueTypeB.exists(tile => tile.id == op.getTileBId && tile.isWaiting )) {

          op.setTileA( currentTileQueueTypeA.find(tile => tile.id == op.getTileAId && tile.isWaiting).get)
          op.setTileB( currentTileQueueTypeB.find(tile => tile.id == op.getTileBId && tile.isWaiting).get)

          interface.sramA.changeTileStateIfExists(op.getTileAId, TileState.calculating)
          interface.sramB.changeTileStateIfExists(op.getTileBId, TileState.calculating)

          op.startCalculation(arrayConfig)
          calculatingOperation += op
        }
      case Some(op) if op.isWaiting =>
        if(capacityLeftTileA() >= arrayConfig.bandwidthOfInputA && capacityLeftTileB() >= arrayConfig.bandwidthOfInputB){
          schedule.find(op => op.isWaiting).get.willCalculateTile()
        }
      case _ =>
        //Do nothing
    }

  }

  private def changeOperationStateWs(interface:Interface): Unit = {
    assert(arrayConfig.dataflow == Dataflow.Ws, "[error] Wrong dataflow need Ws for this function")

    val op = schedule.find(op => op.isNextCalculation || op.isLoading || op.isLoaded || op.isWaiting)

    op match {
      case Some(op) if op.isNextCalculation =>

        if(currentTileQueueTypeB.exists(tile => tile.id == op.getTileBId && tile.isWaiting)) {
          op.setTileB(currentTileQueueTypeB.find(tile => tile.id == op.getTileBId && tile.isWaiting).get)
          interface.sramB.changeTileStateIfExists(op.getTileBId, TileState.loading)
          op.startLoading( arrayConfig.dataflow )

          if (op.getTileB.ownedByArray)
            op.completeLoading(arrayConfig.dataflow)

        }

      case Some(op) if op.isLoading =>

        if (op.getTileB.ownedByArray)
          op.completeLoading(arrayConfig.dataflow)

      case Some(op) if op.isLoaded =>
        //TODO change loaded tile into calculating
        if(currentTileQueueTypeA.exists(tile => tile.id == op.getTileAId && tile.isWaiting && tile.memoryOccupiedByArray > 0 )){
          op.setTileA(currentTileQueueTypeA.find(tile => tile.id == op.getTileAId && tile.isWaiting && tile.memoryOccupiedByArray > 0).get)
          interface.sramA.changeTileStateIfExists(op.getTileAId, TileState.calculating)
          op.startCalculation(arrayConfig)
          calculatingOperation += op
        }

      case Some(op) if op.isWaiting =>

        if(currentTileQueueTypeB.exists(tile => tile.id == op.getTileBId && tile.isLoaded)){

          val tileB = currentTileQueueTypeB.find(tile => tile.id == op.getTileBId && tile.isLoaded).get
          assert(tileB.memoryOccupiedByArray == tileB.dims.memorySize, "[error] Asser this Tile is loaded")
          op.setTileB(tileB)
          op.completeLoading(arrayConfig.dataflow)

        } else if(capacityLeftTileB() >= arrayConfig.bandwidthOfInputB)
          schedule.find(op => op.isWaiting).get.willCalculateTile()

      case _ =>
        //Do nothing
    }

  }

  private def resetTileIdToReceive(): Unit = {
    tileIdToReceiveA = (-1, -1)
    tileIdToReceiveB = (-1, -1)
  }

  private def setTileIdToReceive(): Unit = {
    if(schedule.exists(op => op.needTile(arrayConfig.dataflow))){
      val operation = schedule.find(op => op.needTile(arrayConfig.dataflow)).get
      tileIdToReceiveA = operation.getRequiredTileAId(arrayConfig.dataflow)
      tileIdToReceiveB = operation.getRequiredTileBId(arrayConfig.dataflow)
    }
  }


  //Util
  private def generateOperationId(tileAId: (Int, Int) , tileBId: (Int, Int) ): (Int, Int, Int) = {
    (tileAId._1, tileBId._2, tileAId._2)
  }

  private def capacityLeftTileA(): Int =
    arrayConfig.capacityOfTileA - currentTileQueueTypeA.map(_.memoryOccupiedByArray).sum

  private def capacityLeftTileB(): Int =
    arrayConfig.capacityOfTileB - currentTileQueueTypeB.map(_.memoryOccupiedByArray).sum

  //Util functions
  def printTiles(): Unit = {
    log(s"\t[Systolic Tensor Array]")
    log(s"\t\tTile A ID to receive: $tileIdToReceiveA")
    log(s"\t\tTile B ID to receive: $tileIdToReceiveB")
    log(s"\t\tTarget Operation Tile C id: ${generateOperationId(tileIdToReceiveA, tileIdToReceiveB)}")

    log("")
    log("\t\t[Current tile data type A]")
    if(currentTileQueueTypeA.isEmpty)
      log("\t\tEmpty")
    else
      currentTileQueueTypeA.foreach(tile => tile.printTile())
    log("")

    log("\t\t[Current tile data type B]")
    if(currentTileQueueTypeB.isEmpty)
      log("\t\tEmpty")
    else
      currentTileQueueTypeB.foreach(tile => tile.printTile())
    log("")

    log("\t\t[Current tile data type C]")
    if(calculatingOperation.isEmpty)
      log("\t\tEmpty")
    else
      calculatingOperation.foreach(op => op.getTileC.printTile())
    log("")

    log(s"\t\tCapacity Left TileA: ${capacityLeftTileA()}")
    log(s"\t\tCapacity Left TileB: ${capacityLeftTileB()}")
    log("")

  }

  def printSchedule(): Unit = {
    log("\t[Tile Schedule]")
    schedule.foreach(op => op.printOperation())
  }

  override def isHardwareEmpty: Boolean = {
    currentTileQueueTypeA.isEmpty &&
      currentTileQueueTypeB.isEmpty &&
      nextTileQueueTypeA.isEmpty &&
      nextTileQueueTypeB.isEmpty &&
      calculatingOperation.isEmpty
  }

}

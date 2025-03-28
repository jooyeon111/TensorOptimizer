package simulation

import common.Dataflow
import scala.collection.mutable

//Remember modeling not only modeling the systolic tensor array itself, modeling pre- and post-processor too
final class Array(
  val arrayConfig: ArrayConfig,
  val loggerOption: LoggerOption,
)extends Hardware with Logger {

  setMode(loggerOption)


  var schedule: Vector[ScheduledOperation] = _

  var canArrayReceiveTileFromSramAfterBufferChangeA = false
  var canArrayReceiveTileFromSramAfterBufferChangeB = false

  private var isBufferFlagChangedA = false
  private var isBufferFlagChangedB = false

  private var previousFirstFillUpA = false
  private var previousFirstFillUpB = false

  private var tileIdToReceiveA: (Int,Int) = (-1, -1)
  private var tileIdToReceiveB: (Int,Int) = (-1, -1)

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

  def uploadOperationVector(operationVector: Vector[MultiplicationOperation]): Unit = {

    def createScheduledOperation(
      op: MultiplicationOperation,
      isTileAUsedInNext: Boolean = false,
      isTileBUsedInNext: Boolean = false
    ) = {
      new ScheduledOperation(
        arrayConfig.dataflow,
        op.layerName,
        op.operationId,
        op.generateTileC,
        isTileAUsedInNextOp = isTileAUsedInNext,
        isTileBUsedInNextOp = isTileBUsedInNext,
        arrayConfig.capacityOfTileA,
        arrayConfig.capacityOfTileB,
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

//    incrementWriteAccessCount()

  }


  override def update(interface: Interface) : Unit = {

    markTileSendFailed()
    isBufferFlagChangedA = false
    isBufferFlagChangedB = false

    if (interface.sramA.isFirstFillUpDone && !previousFirstFillUpA) {
      isBufferFlagChangedA = true
      previousFirstFillUpA = true
    }

    if (interface.sramB.isFirstFillUpDone && !previousFirstFillUpB) {
      isBufferFlagChangedB = true
      previousFirstFillUpB = true
    }

    if(isReadyToSend) {

      if (calculatingOperation.nonEmpty) {
        markTileSendSuccessful()
        prepareTileForSend()
        send(interface)
      }

      countSramAccess(interface)
      updateState()
      changeOperationState(interface)
      resetTileIdToReceive()
      setTileIdToReceive()
      calculatingOperation.foreach(op => op.updateTimer())
      countArrayActiveState()
      judgeSramDoubleBufferLogic(interface)
      setRequestedTilesToSRAM(interface)

      if(isAllCalculated){
        interface.sramA.clearBuffer()
        interface.sramB.clearBuffer()
      }

    }



//    }

  }

  private def countArrayActiveState(): Unit = {
    if(calculatingOperation.nonEmpty)
      arrayActiveCount += 1
  }

  private def countSramAccess(interface: Interface): Unit = {
    if(tileIdToReceiveA != (-1, -1) && interface.sramA.isFirstFillUpDone) {
      totalMemoryAccessCountA += 1.0
      if(interface.sramA.hasThisTileInReadBuffer(tileIdToReceiveA, DataType.A) &&
        (capacityLeftTileA() >= arrayConfig.bandwidthOfInputA ||
          (capacityLeftTileA() == 0 && calculatingOperation.exists(_.canBeColoredTileA))) &&
        nextTileQueueTypeA.exists(_.id == tileIdToReceiveA)) {
        totalMemoryHitCountA += 1.0
      } else {
        totalMemoryMissCountA += 1.0
      }
    }

    if(tileIdToReceiveB != (-1, -1) && interface.sramB.isFirstFillUpDone) {
      totalMemoryAccessCountB += 1.0
      if(interface.sramB.hasThisTileInReadBuffer(tileIdToReceiveB, DataType.B) &&
        (capacityLeftTileB() >= arrayConfig.bandwidthOfInputB ||
          (capacityLeftTileB() == 0 && calculatingOperation.exists(_.canBeColoredTileB))) &&
        nextTileQueueTypeB.exists(_.id == tileIdToReceiveB)) {
        totalMemoryHitCountB += 1.0
      } else {
        totalMemoryMissCountB += 1.0
      }
    }
  }

  private def judgeSramDoubleBufferLogic(interface: Interface): Unit = {

//    if(interface.sramA.isFirstFillUpDone)
//      isFirstFillUpDoneA = true

    var hasTileInWriteBufferA = false
    var hasTileInWriteBufferB = false

    if(tileIdToReceiveA != (-1, -1)) {
      if(!isBufferFlagChangedA) {
        if (!interface.sramA.hasThisTileInReadBuffer(tileIdToReceiveA, DataType.A)) {
          if (interface.sramA.hasThisTileInWriteBuffer(tileIdToReceiveA, DataType.A))
            hasTileInWriteBufferA = true

          isBufferFlagChangedA = interface.sramA.canSwapBuffers
          canArrayReceiveTileFromSramAfterBufferChangeA = hasTileInWriteBufferA && isBufferFlagChangedA

        }
      } else {

        if(interface.sramA.hasThisTileInReadBuffer(tileIdToReceiveA, DataType.A))
          canArrayReceiveTileFromSramAfterBufferChangeA = true

      }

    }

    if(tileIdToReceiveB != (-1, -1)) {
      if(!isBufferFlagChangedB){
        if(!interface.sramB.hasThisTileInReadBuffer(tileIdToReceiveB, DataType.B)) {

          if(interface.sramB.hasThisTileInWriteBuffer(tileIdToReceiveB, DataType.B))
            hasTileInWriteBufferB = true

          isBufferFlagChangedB = interface.sramB.canSwapBuffers
          canArrayReceiveTileFromSramAfterBufferChangeB = hasTileInWriteBufferB && isBufferFlagChangedB

        }

      } else {

        if(interface.sramB.hasThisTileInReadBuffer(tileIdToReceiveB, DataType.B))
          canArrayReceiveTileFromSramAfterBufferChangeB = true

      }
    }

  }

  private def setRequestedTilesToSRAM(interface: Interface): Unit = {

    def handleTileARequest(sram: DoubleBufferSram, tileId: (Int, Int), bandwidth: Int): Unit = {
      if (sram.hasThisTileInReadBuffer(tileId, DataType.A) && tileId != (-1, -1)) {
        if (capacityLeftTileA() >= bandwidth) {
          sram.setTileIdToSend(tileId)
        } else if (capacityLeftTileA() == 0) {
          val canSendTile = calculatingOperation.exists(op =>
            op.getTileA.memoryOccupiedByArray > 0 && op.canBeColoredTileA
          )
          if (canSendTile) {
            sram.setTileIdToSend(tileId)
          }
        }
      }
    }

    def handleTileBRequest(sram: DoubleBufferSram, tileId: (Int, Int), bandwidth: Int): Unit = {
      if (sram.hasThisTileInReadBuffer(tileId, DataType.B) && tileId != (-1, -1)) {
        if (capacityLeftTileB() >= bandwidth) {
          sram.setTileIdToSend(tileId)
        } else if (capacityLeftTileB() == 0) {
          val canSendTile = calculatingOperation.exists(op =>
            op.getTileB.memoryOccupiedByArray > 0 && op.canBeColoredTileB
          )
          if (canSendTile) {
            sram.setTileIdToSend(tileId)
          }
        }
      }
    }

    arrayConfig.dataflow match {
      case Dataflow.Is | Dataflow.Ws =>
        handleTileARequest(interface.sramA, tileIdToReceiveA, arrayConfig.bandwidthOfInputA)
        handleTileBRequest(interface.sramB, tileIdToReceiveB, arrayConfig.bandwidthOfInputB)
      case Dataflow.Os =>
        if(interface.sramA.hasThisTileInReadBuffer(tileIdToReceiveA, DataType.A) &&
          interface.sramB.hasThisTileInReadBuffer(tileIdToReceiveB, DataType.B) &&
          tileIdToReceiveA != (-1, -1) &&
          tileIdToReceiveB != (-1, -1)){

          val canRequestA = capacityLeftTileA() >= arrayConfig.bandwidthOfInputA |
            (capacityLeftTileA() == 0 && calculatingOperation.exists(_.canBeColoredTileA))

          val canRequestB = capacityLeftTileB() >= arrayConfig.bandwidthOfInputB |
            (capacityLeftTileB() == 0 && calculatingOperation.exists(_.canBeColoredTileB))

          if(canRequestA && canRequestB){
            interface.sramA.setTileIdToSend(tileIdToReceiveA)
            interface.sramB.setTileIdToSend(tileIdToReceiveB)
          }
        }

    }

//    handleTileARequest(interface.sramA, tileIdToReceiveA, arrayConfig.bandwidthOfInputA)
//    handleTileBRequest(interface.sramB, tileIdToReceiveB, arrayConfig.bandwidthOfInputB)

  }


  override def prepareTileForSend(): Unit = {

    def coloringTileAorB(targetTile: Tile, bandwidth: Int): Unit = {
      if(targetTile.memoryOccupiedByArray - bandwidth >= 0 ){
        targetTile.memoryCalculatedByArray = targetTile.memoryCalculatedByArray + bandwidth
        targetTile.memoryOccupiedByArray = targetTile.memoryOccupiedByArray - bandwidth
      } else {
        targetTile.memoryCalculatedByArray = targetTile.memoryCalculatedByArray + targetTile.memoryOccupiedByArray
        targetTile.memoryOccupiedByArray = 0
      }
    }

    def coloringTileC(targetTileC: Tile): Unit = {
      if(arrayConfig.outputBandwidth - targetTileC.memoryOccupiedByArray >= 0){
        targetTileC.memoryOccupiedBySram = targetTileC.dims.memorySize
        targetTileC.memoryOccupiedByArray = 0
      } else {
        targetTileC.memoryOccupiedBySram += arrayConfig.outputBandwidth
        targetTileC.memoryOccupiedByArray = targetTileC.dims.memorySize - targetTileC.memoryOccupiedBySram
      }
    }

//    calculatingOperation.find(op => op.getTileA.memoryOccupiedByArray > 0) match {
//      case Some(op) if op.isInputATileGoneTimerExpired && !op.isTileAUsedInNextOp =>
//        coloringTileAorB(op.getTileA, arrayConfig.bandwidthOfInputA)
//      case _ =>
//    }
//    calculatingOperation.find(op => op.getTileB.memoryOccupiedByArray > 0 ) match {
//      case Some(op) if op.isInputBTileGoneTimerExpired && !op.isTileBUsedInNextOp =>
//        coloringTileAorB(op.getTileB, arrayConfig.bandwidthOfInputB)
//      case _ =>
//    }

    calculatingOperation.find(_.canBeColoredTileA) match {
      case Some(op) =>
        coloringTileAorB(op.getTileA, arrayConfig.bandwidthOfInputA)
      case _ =>
    }

    calculatingOperation.find(_.canBeColoredTileB) match {
      case Some(op) =>
        coloringTileAorB(op.getTileB, arrayConfig.bandwidthOfInputB)
      case _ =>
    }


    val outputTileC = calculatingOperation.front
    val tileC = outputTileC.getTileC
    if(outputTileC.canGenerateOutputTile)
      coloringTileC(tileC)

  }



  override def send(interface: Interface) : Unit = {

    val operation = calculatingOperation.front
    val targetTileA = operation.getTileA
    val targetTileB = operation.getTileB
    val targetTileC = operation.getTileC

    require(operation.isCalculating, s"[error] Only calculating operation can enter this function" +
      s" ID: ${operation.operationId}")

    if(targetTileC.memoryOccupiedBySram > 0 ){
//      markTileSendSuccessful()

      if(targetTileC.ownedBySram) {

        assert(targetTileA.ownedByArray, s"[error] Array dose not have this tile completely" +
          s" Operation ID: ${operation.operationId} Tile A Id: ${operation.getTileAId}" //+
        )

        assert(targetTileB.ownedByArray, s"[error] Array dose not have this tile completely" +
          s" Operation ID: ${operation.operationId} Tile B Id: ${targetTileB.printTile()}" //+
        )

        operation.completeCalculation()


        val tileAIndex: Int = currentTileQueueTypeA.indexWhere(tile => tile.id == targetTileA.id)
        val tileBIndex: Int = currentTileQueueTypeB.indexWhere(tile => tile.id == targetTileB.id)

        assert(tileAIndex != -1, s"[error] Cannot find tile in array ID: ${operation.operationId}")
        assert(tileBIndex != -1, s"[error] Cannot find tile in array ID: ${operation.operationId}")

        if(operation.isTileAUsedInNextOp)
          currentTileQueueTypeA(tileAIndex).completeLoading()
        else
          currentTileQueueTypeA.remove(tileAIndex)

        if(operation.isTileBUsedInNextOp)
          currentTileQueueTypeB(tileBIndex).completeLoading()
        else
          currentTileQueueTypeB.remove(tileBIndex)


        interface.sramC.receive(targetTileC.copyTile())
        calculatingOperation.dequeue()

      } else
        interface.sramC.receive(targetTileC.copyTile())

    }

  }


  override def updateState(): Unit = {
    def processQueueUpdates[T <: Tile](nextQueue: mutable.Queue[T], currentQueue: mutable.Queue[T]): Unit = {
      while(nextQueue.nonEmpty) {
        if (currentQueue.isEmpty) {
          currentQueue.enqueue(nextQueue.dequeue())
        } else if (currentQueue.last.id == nextQueue.head.id && !currentQueue.last.ownedByArray) {
          currentQueue.last.copyDataTransferData(nextQueue.head)
          nextQueue.dequeue()
        } else {
          currentQueue.enqueue(nextQueue.dequeue())
        }
      }
    }

    processQueueUpdates(nextTileQueueTypeA, currentTileQueueTypeA)
    processQueueUpdates(nextTileQueueTypeB, currentTileQueueTypeB)

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
          val loadingTileA = currentTileQueueTypeA.find(tile => tile.id == op.getTileAId && tile.isWaiting).get
          interface.sramA.changeTileStateIfExists(loadingTileA.id, TileState.loading)
          op.setTileA(loadingTileA)
          op.startLoading()
        }

      case Some(op) if op.isLoading =>

        if (op.getTileA.ownedByArray) {
          op.completeLoading()
        }

      case Some(op) if op.isLoaded =>

        if (currentTileQueueTypeB.exists(tile => tile.id == op.getTileBId && tile.isWaiting)){

          val loadedTileA = op.getTileA
          val calculatingTileB = currentTileQueueTypeB.find(tile => tile.id == op.getTileBId && tile.isWaiting).get

          interface.sramB.changeTileStateIfExists(op.getTileBId, TileState.calculating)
          op.setTileB(calculatingTileB)
          op.startCalculation(arrayConfig)

          val opIndex = schedule.indexOf(op) + 1
          schedule.slice(opIndex, schedule.length)
            .filter{nextOp =>
              (nextOp.isWaiting || nextOp.isNextCalculation) && nextOp.getTileAId == loadedTileA.id
            }
            .foreach { nextOp =>
              nextOp.setTileA(loadedTileA)
              nextOp.completeLoading()
            }

          calculatingOperation += op

        }

      case Some(op) if op.isWaiting =>
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
        schedule.find(op => op.isWaiting).get.willCalculateTile()

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
          val loadingTileB = currentTileQueueTypeB.find( tile => tile.id == op.getTileBId && tile.isWaiting).get
          interface.sramB.changeTileStateIfExists(loadingTileB.id, TileState.loading)
          op.setTileB(loadingTileB)
          op.startLoading()
        }

      case Some(op) if op.isLoading =>

        if (op.getTileB.ownedByArray)
          op.completeLoading()

      case Some(op) if op.isLoaded =>
        if(currentTileQueueTypeA.exists(tile => tile.id == op.getTileAId && tile.isWaiting)){

          val loadedTileB = op.getTileB
          val calculatingTileA = currentTileQueueTypeA.find(tile => tile.id == op.getTileAId && tile.isWaiting).get

          interface.sramA.changeTileStateIfExists(op.getTileAId, TileState.calculating)
          op.setTileA(calculatingTileA)
          op.startCalculation(arrayConfig)

          val opIndex = schedule.indexOf(op) + 1
          schedule.slice(opIndex, schedule.length)
            .filter{ nextOp =>
              (nextOp.isWaiting || nextOp.isNextCalculation) && nextOp.getTileBId == loadedTileB.id
            }
            .foreach{ nextOp =>
              nextOp.setTileB(loadedTileB)
              nextOp.completeLoading()
            }

          calculatingOperation += op

        }

      case Some(op) if op.isWaiting =>
        schedule.find( op => op.isWaiting).get.willCalculateTile()

      case _ =>
        //Do nothing
    }

  }

  private def resetTileIdToReceive(): Unit = {
    tileIdToReceiveA = (-1, -1)
    tileIdToReceiveB = (-1, -1)
  }

  private def setTileIdToReceive(): Unit = {
    if(schedule.exists(op => op.needTile)){
      val operation = schedule.find(op => op.needTile).get
      tileIdToReceiveA = operation.getRequiredTileAId
      tileIdToReceiveB = operation.getRequiredTileBId
    }
  }

  //Util
  private def generateOperationId(tileAId: (Int, Int) , tileBId: (Int, Int) ): (Int, Int, Int) = {
    (tileAId._1, tileBId._2, tileAId._2)
  }

  def capacityLeftTileA(): Int =
    arrayConfig.capacityOfTileA - currentTileQueueTypeA.map(_.memoryOccupiedByArray).sum

  def capacityLeftTileB(): Int =
    arrayConfig.capacityOfTileB - currentTileQueueTypeB.map(_.memoryOccupiedByArray).sum

  //Util functions
  def printTiles(): Unit = {
    log(s"\t[Systolic Tensor Array]")
    log(s"\t\tChange SRAM A buffer: ${isBufferFlagChangedA}")
    log(s"\t\tChange SRAM B buffer: ${isBufferFlagChangedB}")
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
    else {
      calculatingOperation.foreach { op =>
        op.getTileC.printTile()
        if (op.canBeColoredTileA)
          log(s"\t\tCan Tile A be released? ${op.canBeColoredTileA}")
        if (op.canBeColoredTileB)
          log(s"\t\tCan Tile B be released? ${op.canBeColoredTileB}")
        if (op.isTileAUsedInNextOp)
          log(s"\t\tIs A Used in Next Op ${op.isTileAUsedInNextOp}")
        if (op.isTileBUsedInNextOp)
          log(s"\t\tIs B Used in Next Op ${op.isTileBUsedInNextOp}")
      }
    }
    log("")
    log(s"\t\tCapacity Left TileA: ${capacityLeftTileA()}")
    log(s"\t\tCapacity Left TileB: ${capacityLeftTileB()}")
    log("")

  }


  def printSchedule(): Unit = {
    log("\t[Tile Schedule]")
    schedule.foreach(op => op.printSchedule())
  }

  override def isHardwareEmpty: Boolean = {
    currentTileQueueTypeA.isEmpty &&
      currentTileQueueTypeB.isEmpty &&
      nextTileQueueTypeA.isEmpty &&
      nextTileQueueTypeB.isEmpty &&
      calculatingOperation.isEmpty
  }

}

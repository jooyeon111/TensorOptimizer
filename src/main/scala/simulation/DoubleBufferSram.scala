package simulation

import simulation.DataType.DataType
import simulation.TileState.TileState
import scala.collection.mutable

class DoubleBufferSram(
  override val dataType: DataType,
  val outputBandwidth: Int,
  override val singleBufferTileCapacity: Int,
  override val singleBufferLimitKb: Int,
  val referenceData: Option[SramReferenceData] = None,
  val loggerOption: LoggerOption,
) extends Sram with AccessCounter{


  require(outputBandwidth >= 1, "[error] Output bandwidth must be at least 1")
  require(singleBufferTileCapacity >=  1, "[error] Tile capacity must be at least 1")
  setMode(loggerOption)

  private var swapCount = 0

  private var tileIdToSend: (Int, Int) = (-1, -1)
  private val tileOperationOrder: mutable.ListBuffer[TileScheduleEntry] = mutable.ListBuffer.empty[TileScheduleEntry]
  private var availableOutputBandwidth: Int = outputBandwidth

  private var totalDramAccessCount: Double = 0.0
  private var totalDramHitCount: Double = 0.0
  private var totalDramMissCount: Double = 0.0

  def getDramAccessCount: Double = totalDramAccessCount
  def getDramHitCount: Double = totalDramHitCount
  def getDramMissCount: Double = totalDramMissCount

  def getSramReadEnergy: Option[Double] =
    referenceData.map( data => getReadAccessCount * data.readEnergyPj )

  def getSramWriteEnergy: Option[Double] =
    referenceData.map( data => getWriteAccessCount * data.writeEnergyPj )

  def getReadBufferIDs: mutable.Queue[(Int, Int)] = {
    readBuffer.map(tile =>tile.id.asInstanceOf[(Int,Int)])
  }

  private def isWriteBufferTilesIntact: Boolean = {
    writeBuffer.forall(_.ownedBySram)
  }

  private def isNothingToUpdateInWriteBuffer: Boolean = {
    writePendingBuffer.isEmpty
  }

  //Function called by compiler

  //TODO integrate below two functions
  def initTileSchedule(operationVector: Vector[MultiplicationOperation]): Unit = {
    operationVector.foreach{ operation =>
      dataType match {
        case DataType.A =>
          tileOperationOrder += TileScheduleEntry(operation.getTileAId)
        case DataType.B =>
          tileOperationOrder += TileScheduleEntry(operation.getTileBId)
        case DataType.C =>
          Console.err.println("[error] SRAM Tile Schedule Uploading fails")
          sys.exit(1)
      }
    }

  }

  //Functions are called by DRAM
  def existsInInBuffers(targetTile: Tile): Boolean = {
    assert(dataType == targetTile.dataType, "[error] Tile data type mismatch")
    readBuffer.exists(tile => tile.id == targetTile.id) || writeBuffer.exists(tile => tile.id == targetTile.id)
  }

  def howManyTileCanStore() : Int = {
    singleBufferTileCapacity - writeBuffer.length
  }

  def receive(tiles: mutable.Queue[Tile]): Unit = {

    tiles.foreach( tile => assert(tile.dataType == dataType,
      "[error] received tile data type and SRAM tile data dose not match"))

    while(tiles.nonEmpty){

      assert(tiles.front.memoryOccupiedBySram > 0, s"[error] SRAM $dataType cannot take this tile ${tiles.front.id}")
      assert(tiles.front.dataType == dataType, s"[error] SRAM $dataType data type and tile data type do not match")

      writePendingBuffer += tiles.dequeue()

    }

    incrementWriteAccessCount()

  }

  //Functions are called by array
  def setTileIdToSend( nextTileIdToSend: (Int, Int) ): Unit =
    tileIdToSend = nextTileIdToSend

  def hasThisTileInReadBuffer(tileId: (Int,Int), tileDataType: DataType): Boolean = {
    assert(dataType == tileDataType, "[error] Tile data type mismatch")
    readBuffer.exists(_.id == tileId)
  }

  def hasThisTileInWriteBuffer(tileId: (Int,Int), tileDataType: DataType): Boolean = {
    assert(dataType == tileDataType, "[error] Tile data type mismatch")
    writeBuffer.exists(_.id == tileId)
  }

  def canSwapBuffers: Boolean = {
    require(isNothingToUpdateInWriteBuffer, "[error] There is something to update in write buffer")

    if(isFirstFillUpDone){
      if(isWriteBufferTilesIntact){
        updateToReadBuffer(writeBuffer)
        updateToWriteBuffer(readBuffer)
        executeBufferSwap()
        increaseBufferSwapCount()
        swapCount += 1
        true
      } else {
        increaseBufferSwapStallCount()
        false
      }
    } else
      false

  }

  def clearBuffer(): Unit = {
    readBuffer.clear()
    writeBuffer.clear()
  }

  //TODO delete after change operation state in array
  def changeTileStateIfExists(id: (Int, Int), tileState: TileState): Unit = {
    if(readBuffer.exists( tile => tile.id == id && tile.memoryOccupiedByArray > 0)){
      val tile = readBuffer.find(tile => tile.id == id).get

      tileState match {
        case TileState.loading =>
          tile.startLoading()

        case TileState.calculating =>
          tile.startCalculation()

        case _ =>
          Console.err.println(s"[error] Cannot change tie into this tate $tileState")
          sys.exit(1)
      }
    }
  }

  //Functions are called by interface
//  def isFirstFillUpCompete: Boolean = isFirstFillUpDone

  override def update(interface: Interface) : Unit = {

    markTileSendFailed()

    if(isGoodStateToGo && isReadyToSend){
      prepareTileForSend()
      send(interface)
      resetTileIdToSend()
    }

    countDramAccess()
    updateState()

    if(!isFirstFillUpDone)
      checkFirstFillUp(interface)

    updateMemoryMonitor()

  }


  private def isGoodStateToGo: Boolean =
    tileIdToSend != (-1, -1)

  override def prepareTileForSend(): Unit = {

    def coloringTile(tile: Tile): Unit = {
      if(availableOutputBandwidth - tile.memoryOccupiedBySram >= 0){
        availableOutputBandwidth = availableOutputBandwidth - tile.memoryOccupiedBySram
        tile.memoryOccupiedByArray = tile.dims.memorySize
        tile.memoryOccupiedBySram = 0
      } else {
        tile.memoryOccupiedByArray += availableOutputBandwidth
        tile.memoryOccupiedBySram = tile.dims.memorySize - tile.memoryOccupiedByArray
      }
    }

    assert(readBuffer.nonEmpty, "[error] Current read buffer is empty")

    if(readBuffer.exists(tile => tile.id == tileIdToSend)){
      val sendingTile = readBuffer.find(tile => tile.id == tileIdToSend).get
      coloringTile(sendingTile)
    } else {
      Console.err.println(s"[error] Sending tile dose not exist in current read buffer")
      sys.exit(1)
    }

    availableOutputBandwidth = outputBandwidth

  }



  override def send(interface: Interface): Unit = {

    def sendTile(tile: Tile): Unit = {
      interface.array.receive(tile.copyTile())
      if (tile.ownedByArray) {
        tile.reallocateToSram()
      }
      incrementReadAccessCount()
    }

    //TODO do we need buffer here?
    def sendFromBuffer(buffer: mutable.Queue[Tile]): Unit = {
      if (buffer.nonEmpty) {
        markTileSendSuccessful()
        buffer.find(_.memoryOccupiedByArray > 0).foreach(sendTile)
      }
    }

    sendFromBuffer(readBuffer)
  }

  override def updateState(): Unit = {
    def updateBuffer(buffer: mutable.Queue[Tile], pending: mutable.Queue[Tile]): Unit = {
      while(pending.nonEmpty){
        val newTile = pending.dequeue()
        if(buffer.isEmpty){
          buffer += newTile
        } else if (buffer.last.id == newTile.id && !buffer.last.ownedBySram){
          buffer.remove(buffer.length - 1)
          buffer += newTile
        } else {
          buffer += newTile
        }
      }
    }

    updateBuffer(readBuffer, readPendingBuffer)
    updateBuffer(writeBuffer, writePendingBuffer)

  }

  private def resetTileIdToSend(): Unit =
    tileIdToSend = (-1, -1)

  private def checkFirstFillUp(interface: Interface): Unit = {

    val otherSram = dataType match {
      case DataType.A => interface.sramB
      case DataType.B => interface.sramA
      case _ =>
        Console.err.println(s"[error] Wrong SRAM Type")
        sys.exit(1)
    }

    val shouldSwitch = {

      val isWriteBufferFull = isBufferIntactAndFull(writeBuffer)
      val isBufferStateReady = isWriteBufferFull || interface.dram.isHardwareEmpty

      val isOtherSramWriteBufferTilesIntact = otherSram.isWriteBufferTilesIntact
      val isOtherSramWriteBufferNothingToUpdate = otherSram.isNothingToUpdateInWriteBuffer

      isBufferStateReady && isOtherSramWriteBufferTilesIntact && isOtherSramWriteBufferNothingToUpdate

    }

    if (shouldSwitch) {

      updateToReadBuffer(writeBuffer)
      assert(readBuffer.isEmpty,"Read buffer is not empty in check first fill up")
      executeBufferSwap()
      isFirstFillUpDone = true
      firstFillUpCycle = interface.getCycle

      otherSram.updateToReadBuffer(otherSram.writeBuffer)
      assert(otherSram.readBuffer.isEmpty,"Read buffer is not empty in check first fill up")
      otherSram.executeBufferSwap()
      otherSram.isFirstFillUpDone = true
      otherSram.firstFillUpCycle = interface.getCycle


    }

  }

  private def countDramAccess(): Unit = {
    val unscheduledTiles = tileOperationOrder.filter(!_.isInReadBuffer).map(_.id).toSet
    if(unscheduledTiles.nonEmpty){
      val bufferIds = (readBuffer.map(_.id.asInstanceOf[(Int, Int)]) ++ writeBuffer.map(_.id.asInstanceOf[(Int, Int)])).toSet
      if(!unscheduledTiles.subsetOf(bufferIds)){
        totalDramAccessCount += 1.0
        if(writePendingBuffer.nonEmpty){
          totalDramHitCount += 1.0
        } else {
          totalDramMissCount += 1.0
        }
      }
    }
  }

//  private def updateToReadBuffer(writeBuffer: mutable.Queue[Tile]): Unit = {
//    if(writeBuffer.isEmpty) return
//
//    while(tileOperationOrder.exists(!_.isInReadBuffer)){
//
//      val unScheduledTileId = tileOperationOrder.find(!_.isInReadBuffer).get.id
//      val startIdx = writeBuffer.indexWhere(_.id == unScheduledTileId)
//      if(startIdx < 0 ) return
//
//      val writeBufferPattern = mutable.Queue[(Int, Int)]()
//      writeBufferPattern ++= writeBuffer.slice(startIdx, writeBuffer.size).map(_.id.asInstanceOf[(Int, Int)])
//
//      matchPatterns(writeBufferPattern, tileOperationOrder)
//
//    }
//
//  }

  private def updateToReadBuffer(writeBuffer: mutable.Queue[Tile]): Unit = {
    if(writeBuffer.isEmpty) return

    while(tileOperationOrder.exists(!_.isInReadBuffer)){

      val writeBufferIds = writeBuffer
        .map(_.id)
        .toSet

      val pendingOperation = tileOperationOrder
        .find(!_.isInReadBuffer)
        .get

      if(writeBufferIds.contains(pendingOperation.id)){
        pendingOperation.markAsInReadBuffer()
      } else {
        return
      }

    }

  }

//  private def updateToWriteBuffer(readBuffer: mutable.Queue[Tile]): Unit = {
//
//    if(tileOperationOrder.forall(_.isInReadBuffer))
//      return
//
//    val readBufferIds = readBuffer.map(_.id).toSet
//    val unscheduledNeededTiles = tileOperationOrder
//      .filter(!_.isInReadBuffer)
//      .map(_.id)
//      .filter(readBufferIds.contains)
//      .toSet
//
//    val indicesToKeep = readBuffer.indices
//      .filter(i => unscheduledNeededTiles.contains(readBuffer(i).id.asInstanceOf[(Int, Int)]))
//      .toSet
//
//    for (i <- readBuffer.indices.reverse if !indicesToKeep.contains(i)) {
//      readBuffer.remove(i)
//    }
//
//  }

  private def updateToWriteBuffer(readBuffer: mutable.Queue[Tile]): Unit = {

    if(tileOperationOrder.forall(_.isInReadBuffer))
      return

    //Delete Tiles Which are not going to use forever
    val readBufferIds = readBuffer.map(_.id).toSet

    val pendingMatchingIds = tileOperationOrder
      .filter(!_.isInReadBuffer)
      .map(_.id)
      .filter(readBufferIds.contains)
      .toSet

    val indicesToKeep = readBuffer
      .indices
      .filter(i => pendingMatchingIds.contains(readBuffer(i).id.asInstanceOf[(Int, Int)]))
      .toSet

    for (i <- readBuffer.indices.reverse if !indicesToKeep.contains(i))
      readBuffer.remove(i)

    val pendingOperations = tileOperationOrder
      .filter(!_.isInReadBuffer)

    val firstMatchingIndex = pendingOperations
      .indexWhere(tile => readBufferIds.contains(tile.id))

    if(firstMatchingIndex == -1)
      return

    val operationsBeforeMatch = if (firstMatchingIndex > 0) {
      pendingOperations.take(firstMatchingIndex)
    } else {
      Seq.empty
    }

    val operationBeforeMatchSet = operationsBeforeMatch.toSet
    val tileNumberToBeLoaded = operationBeforeMatchSet.size

    while (readBuffer.nonEmpty && readBuffer.size + tileNumberToBeLoaded > singleBufferTileCapacity) {
      readBuffer.dequeue()
//      readBuffer.removeLast()
    }

  }

  //Utility function
  def printTiles(): Unit = {
    printSram()
    log(s"\t\tTile ID to send $tileIdToSend")
    log("")

  }

  def printSchedule(): Unit = {
    log(s"\t[SRAM $dataType]")
    tileOperationOrder.foreach{ entry =>
      log(s"\t\tID: ${entry.id} Status: ${entry.isInReadBuffer}")
    }
    log(s"")
  }

}
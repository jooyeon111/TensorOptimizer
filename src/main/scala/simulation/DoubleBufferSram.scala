package simulation

import simulation.DataType.DataType
import simulation.TileState.TileState

import scala.annotation.tailrec
import scala.collection.mutable


class DoubleBufferSram(
  override val dataType: DataType,
  val outputBandwidth: Int,
  override val singleBufferTileCapacity: Int,
  override val singleBufferLimitKb: Int,
  val loggerOption: LoggerOption,
) extends Sram with AccessCounter{

  private case class TileScheduleEntry(
    id: (Int,Int),
    var isScheduled: Boolean = false,
  ) {
    def markAsScheduled(): Unit = {
      isScheduled = true
    }
  }

  require(outputBandwidth >= 1, "[error] Output bandwidth must be at least 1")
  require(singleBufferTileCapacity >=  1, "[error] Tile capacity must be at least 1")
  setMode(loggerOption)

  var isSramStall = false

  private var tileIdToSend: (Int, Int) = (-1, -1)
  private val tileOperationOrder: mutable.ListBuffer[TileScheduleEntry] = mutable.ListBuffer.empty[TileScheduleEntry]
  private var availableOutputBandwidth: Int = outputBandwidth

  private var totalDramAccessCount: Double = 0.0
  private var totalDramHitCount: Double = 0.0
  private var totalDramMissCount: Double = 0.0

  def getDramAccessCount: Double = totalDramAccessCount
  def getDramHitCount: Double = totalDramHitCount
  def getDramMissCount: Double = totalDramMissCount


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
  def existsInBuffers(targetTile: Tile): Boolean = {
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

  def hasThisTileForArray(tileId: (Int,Int), tileDataType: DataType): Boolean = {
    assert(dataType == tileDataType, "[error] Tile data type mismatch")
    readBuffer.exists(_.id == tileId)
  }

  def canSwapBuffers: Boolean = {
    require(isNothingToUpdateInWriteBuffer, "[error] There is something to update in write buffer")

//    if(isFirstFillUpCompete && isWriteBufferTilesIntact){
//      updateToReadBuffer(writeBuffer)
//      updateToWriteBuffer(readBuffer)
//      executeBufferSwap()
//      increaseBufferSwapCount()
//    } else {
//      increaseBufferSwapStallCount()
//    }

    if(isFirstFillUpDone){
      if(isWriteBufferTilesIntact){
        updateToReadBuffer(writeBuffer)
        updateToWriteBuffer(readBuffer)
        executeBufferSwap()
        increaseBufferSwapCount()
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

    if(isGoodStateToGo && !isSramStall){
      prepareTileForSend()
      send(interface)
      resetTileIdToSend()
    } else
      stuck = true

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

    def sendFromBuffer(buffer: mutable.Queue[Tile]): Unit = {
      if (buffer.nonEmpty) {
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
      executeBufferSwap()
      isFirstFillUpDone = true
      firstFillUpCycle = interface.getCycle

      otherSram.updateToReadBuffer(otherSram.writeBuffer)
      otherSram.executeBufferSwap()
      otherSram.isFirstFillUpDone = true
      otherSram.firstFillUpCycle = interface.getCycle


    }

  }

  private def countDramAccess(): Unit = {
    val unscheduledTiles = tileOperationOrder.filter(!_.isScheduled).map(_.id).toSet
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

//  private def updateToReadBuffer(writeBuffer: ArrayBuffer[Tile]): Unit = {
//
//    if(writeBuffer.isEmpty) return
//
//    val writeBufferPattern = mutable.Queue[(Int,Int)]()
//    writeBufferPattern ++= writeBuffer.map(tile => tile.id.asInstanceOf[(Int,Int)])
//
//    while(writeBufferPattern.nonEmpty){
//
//      val startIndex = tileOperationOrder.indexWhere(!_.isScheduled)
//      var index = startIndex
//      if(index < 0) return
//
//      if ( writeBufferPattern.head == tileOperationOrder(index).id ) {
//        var patternIndex = 0
//
//        while(patternIndex < writeBufferPattern.size){
//          val tileId = writeBufferPattern(patternIndex)
//          while(tileId == tileOperationOrder(index).id){
//            tileOperationOrder(index).markAsScheduled()
//            index +=1
//          }
//          patternIndex +=1
//        }
//
//      } else {
//        Console.err.println(s"[error] Change write buffer to read buffer malfunction")
//        sys.exit(1)
//      }
//
//      val schedulePattern = mutable.Queue[(Int, Int)]()
//      schedulePattern ++= tileOperationOrder.slice(startIndex, index).map(_.id)
//      val patternSize = schedulePattern.size
//      index = startIndex + patternSize
//
//      @tailrec
//      def findAndMarkPattern(index: Int): Unit = {
//        if(index + patternSize <= tileOperationOrder.length){
//          val isMatched = (0 until patternSize).forall{ i =>
//            tileOperationOrder(index + i).id == tileOperationOrder(startIndex+i).id
//          }
//          if(isMatched){
//            (0 until patternSize).foreach{ i =>
//              tileOperationOrder(index+i).markAsScheduled()
//            }
//            findAndMarkPattern(index+ patternSize)
//          }
//        }
//      }
//
//      findAndMarkPattern(index)
//      (0 until schedulePattern.toSet.size).foreach( _ => writeBufferPattern.dequeue())
//
//    }
//
//  }

  private def updateToReadBuffer(writeBuffer: mutable.Queue[Tile]): Unit = {
    if(writeBuffer.isEmpty) return

    val writeBufferPattern = mutable.Queue[(Int,Int)]()
    writeBufferPattern ++= writeBuffer.map(_.id.asInstanceOf[(Int,Int)])

    while(writeBufferPattern.nonEmpty) {
      val startIndex = tileOperationOrder.indexWhere(!_.isScheduled)
      if(startIndex < 0) return

      if(writeBufferPattern.head != tileOperationOrder(startIndex).id) {
        Console.err.println(s"[error] Change write buffer to read buffer malfunction")
        sys.exit(1)
      }

      var index = startIndex
      var patternIndex = 0

      while(patternIndex < writeBufferPattern.size) {
        val tileId = writeBufferPattern(patternIndex)
        while(index < tileOperationOrder.length && tileId == tileOperationOrder(index).id) {
          tileOperationOrder(index).markAsScheduled()
          index += 1
        }
        patternIndex += 1
      }

      val schedulePattern = mutable.Queue[(Int, Int)]()
      val patternRange = startIndex until index
      schedulePattern ++= patternRange.map(i => tileOperationOrder(i).id)
      val patternSize = schedulePattern.size

      @tailrec
      def findAndMarkPattern(currentIndex: Int): Unit = {
        if(currentIndex + patternSize <= tileOperationOrder.length) {
          val isMatched = (0 until patternSize).forall(i =>
            tileOperationOrder(currentIndex + i).id == tileOperationOrder(startIndex + i).id
          )
          if(isMatched) {
            (0 until patternSize).foreach(i =>
              tileOperationOrder(currentIndex + i).markAsScheduled()
            )
            findAndMarkPattern(currentIndex + patternSize)
          }
        }
      }

      findAndMarkPattern(index)

      (0 until schedulePattern.toSet.size).foreach(_ => writeBufferPattern.dequeue())
    }
  }

  private def updateToWriteBuffer(readBuffer: mutable.Queue[Tile]): Unit = {
    tileOperationOrder.find(!_.isScheduled).foreach { startTileId =>
      val indexToKeep = readBuffer.indexWhere(_.id == startTileId.id)
      if(indexToKeep >= 0){
        readBuffer.remove(0, indexToKeep)
      } else {
        readBuffer.clear()
      }
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
      log(s"\t\tID: ${entry.id} Status: ${entry.isScheduled}")
    }
    log(s"")
  }

//  def printReceivingOrder(): Unit = {
//    log(s"\t[SRAM $dataType]")
//    tileReceivingOrder.foreach { entry =>
//      log(s"\t\tID: ${entry.id} Status: ${entry.isReceived}")
//    }
//    log(s"")
//  }

}
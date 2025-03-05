package simulation

import simulation.DataType.DataType

import scala.collection.mutable
import scala.util.control.Breaks

class OutputDoubleBufferSram(
  val outputBandwidth: Int,
  override val singleBufferTileCapacity: Int,
  override val singleBufferLimitKb: Int,
  val loggerOption: LoggerOption,
) extends Sram with AccessCounter{

  setMode(loggerOption)

  override val dataType: DataType = DataType.C
  private var availableOutputBandwidth: Int = outputBandwidth

  def receive(tile: Tile): Unit = {

    assert(tile.dataType == dataType, "[error] Received tile data type and SRAM tile dat dose not match")

    if(tile.ownedBySram)
      assert(tile.isCalculated, "[error] Only calculated tile C can enter SRAM C")

    writePendingBuffer.enqueue(tile)
    incrementWriteAccessCount()

  }

  override def update(interface: Interface): Unit = {

    prepareTileForSend()
    send(interface)

    updateState()

    if(!isFirstFillUpDone){
      checkFirstFillUp(interface)
    } else {
      swapBuffers(interface)
    }

    judgeDramReadWriteState(interface)
    updateMemoryMonitor()

  }

  override def prepareTileForSend(): Unit = {

    for (i <- readBuffer.indices if !(availableOutputBandwidth == 0)) {

      if ((availableOutputBandwidth - readBuffer(i).memoryOccupiedBySram) >= 0) {
        availableOutputBandwidth = availableOutputBandwidth - readBuffer(i).memoryOccupiedBySram
        readBuffer(i).memoryOccupiedByDram = readBuffer(i).dims.memorySize
        readBuffer(i).memoryOccupiedBySram = 0
      } else {
        readBuffer(i).memoryOccupiedByDram += availableOutputBandwidth
        readBuffer(i).memoryOccupiedBySram =
          readBuffer(i).dims.memorySize - readBuffer(i).memoryOccupiedByDram
        availableOutputBandwidth = 0
      }

    }

    availableOutputBandwidth = outputBandwidth

  }

  override def send(interface: Interface): Unit = {

    val temporalTileQueue = mutable.Queue.empty[Tile]

    val loop = new Breaks
    loop.breakable {

      while (readBuffer.nonEmpty) {
        if (readBuffer.front.memoryOccupiedByDram > 0) {
          if (readBuffer.front.ownedByDram) {

            temporalTileQueue += readBuffer.front
            readBuffer.dequeue()

          } else {
            temporalTileQueue += readBuffer.front.copyTile()
            loop.break()
          }
        } else {
          loop.break()
        }
      }

    }

    if (temporalTileQueue.nonEmpty) {
      interface.dram.receive()
      incrementReadAccessCount()
      markTileSendSuccessful()
    } else
      markTileSendFailed()

  }

  override def updateState(): Unit = {
    def updateBuffer(buffer: mutable.Queue[Tile], pendingBuffer: mutable.Queue[Tile]): Unit = {
      while(pendingBuffer.nonEmpty){
        if(buffer.isEmpty){
          buffer.enqueue(pendingBuffer.dequeue())
        } else {
          val shouldReplaceLast = buffer.last.id == pendingBuffer.head.id && !buffer.last.ownedBySram
          if(shouldReplaceLast) {
            buffer.removeLast()
          }
          buffer.enqueue(pendingBuffer.dequeue())
        }
      }
    }
    updateBuffer(readBuffer, readPendingBuffer)
    updateBuffer(writeBuffer, writePendingBuffer)
  }

  private def checkFirstFillUp(interface: Interface): Unit = {
    if (!isFirstFillUpDone) {
      if (isBufferIntactAndFull(writeBuffer) || interface.array.isAllCalculated) {
        executeBufferSwap()
        firstFillUpCycle = interface.getCycle
        isFirstFillUpDone = true
      }
    }
  }

  private def swapBuffers(interface: Interface): Unit = {

    if(readBuffer.isEmpty){
      if(writeBuffer.forall(tile => tile.ownedBySram)){
        executeBufferSwap()
        increaseBufferSwapCount()
        resumeHardware(interface)
      } else {
        increaseBufferSwapStallCount()
      }
    } else {
      if(writeBuffer.forall(tile => tile.ownedBySram)) {
        if(writeBuffer.length == singleBufferTileCapacity){
          pauseHardware(interface)
        } else {
          resumeHardware(interface)
        }
      }
    }

    if(interface.array.isAllCalculated){
      if(readBuffer.isEmpty && writeBuffer.nonEmpty && writeBuffer.last.ownedBySram) {
        executeBufferSwap()
        increaseBufferSwapCount()
      }
    }

  }

  private def pauseHardware(interface: Interface): Unit = {
//    interface.array.isArrayStall = true
//    interface.sramA.isSramStall = true
//    interface.sramB.isSramStall = true

    interface.array.pauseTileSending()
    interface.sramA.pauseTileSending()
    interface.sramB.pauseTileSending()
  }

  private def resumeHardware(interface: Interface): Unit = {
//    interface.array.isArrayStall = false
//    interface.sramA.isSramStall = false
//    interface.sramB.isSramStall = false

    interface.array.resumeTileSending()
    interface.sramA.resumeTileSending()
    interface.sramB.resumeTileSending()

  }

  private def judgeDramReadWriteState(interface: Interface): Unit = {
    if(readBuffer.nonEmpty)
      interface.dram.pauseTileSending()
    else
      interface.dram.resumeTileSending()

  }

  def printTiles(): Unit = {
    printSram()
  }

}

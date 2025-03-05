package simulation

import simulation.DataType.DataType

import scala.collection.mutable

trait Sram extends Hardware with Logger {

  //TODO unify name toggle or swap
  //Abstract members
  val dataType: DataType
  val singleBufferLimitKb: Int
  protected val singleBufferTileCapacity: Int

  //Memory configuration
  protected val sramTotalSizeBit: Double = singleBufferLimitKb.toDouble * 2.0 * 8.0 * 1024.0

  //Double buffer state
  private var bufferSwapCount: Int = 0
  private var bufferSwapStallCount: Int = 0
  private var isWritingToPong: Boolean = false
  var isFirstFillUpDone: Boolean = false
  protected var firstFillUpCycle: Long = 0

  //memory monitoring
  private var accumulatedMemoryUsageBit: Double = 0.0
  private var accumulatedMemoryUtilization: Double = 0.0

  //Buffers
  private val pingBuffer: mutable.Queue[Tile] = mutable.Queue.empty[Tile]
  private val pendingPingBuffer: mutable.Queue[Tile] = mutable.Queue.empty[Tile]
  private val pongBuffer: mutable.Queue[Tile] = mutable.Queue.empty[Tile]
  private val pendingPongBuffer: mutable.Queue[Tile] = mutable.Queue.empty[Tile]

  //Buffer access
  final protected def readBuffer: mutable.Queue[Tile] = if(isWritingToPong) pingBuffer else pongBuffer
  final protected def writeBuffer: mutable.Queue[Tile]= if(isWritingToPong) pongBuffer else pingBuffer
  final protected def readPendingBuffer: mutable.Queue[Tile] = if(isWritingToPong) pendingPingBuffer else pendingPongBuffer
  final protected def writePendingBuffer: mutable.Queue[Tile] = if(isWritingToPong) pendingPongBuffer else pendingPingBuffer

  //Buffer operations
  final protected def executeBufferSwap(): Unit = {
    isWritingToPong = !isWritingToPong
  }

  final protected def isBufferIntactAndFull(buffer: mutable.Queue[Tile]): Boolean = {
    if (buffer.forall(tile => tile.ownedBySram)
      && (singleBufferTileCapacity - buffer.length == 0))
      true
    else
      false
  }

  //Memory monitoring
  final protected def updateMemoryMonitor(): Unit = {
    accumulatedMemoryUsageBit += calculateMemoryUsage()
    accumulatedMemoryUtilization += calculateMemoryUtilization()
  }

  final private def calculateMemoryUsage(): Double = {
    pingBuffer.map(_.dims.memorySize).sum +
      pongBuffer.map(_.dims.memorySize).sum
  }

  final private def calculateMemoryUtilization(): Double = {
    calculateMemoryUsage() / sramTotalSizeBit * 100.0
  }

  //public API
  final def getFirstFillUpCycle: Long = firstFillUpCycle
  final def getBufferSwapCount: Int = bufferSwapCount
  final def getBufferSwapStallCount: Int = bufferSwapStallCount
  final def increaseBufferSwapCount(): Unit = bufferSwapCount += 1
  final def increaseBufferSwapStallCount(): Unit = bufferSwapStallCount += 1
  final def getAccumulatedMemoryUsage: Double = accumulatedMemoryUsageBit
  final def getAccumulateMemoryUtilization: Double = accumulatedMemoryUtilization

  //Overriding from Hardware
  final override def isHardwareEmpty: Boolean =
    readBuffer.isEmpty &&
      readPendingBuffer.isEmpty &&
      writeBuffer.isEmpty &&
      writePendingBuffer.isEmpty


  final def printSram(): Unit = {
    if(dataType == DataType.A)
      log(s"\t[Input Double Buffer SRAM A]")
    else if(dataType == DataType.B)
      log(s"\t[Input Double Buffer SRAM B]")
    else if(dataType == DataType.C){
      log(s"\t[Output Double Buffer SRAM]")
    } else {
      Console.err.println(s"Wrong data type for input double buffer SRAM")
      sys.exit(1)
    }


    logWithoutNewLine("\t\tFirst fill up is done: ")
    if(isFirstFillUpDone){
      log("YES")
    } else{
      log("NO")
    }
    log("")

    if(dataType == DataType.A | dataType == DataType.B){
      if(isWritingToPong)
        log("\t\t[Buffer0: ARRAY is reading]")
      else
        log("\t\t[Buffer0: DRAM is writing]")
    } else if(dataType == DataType.C) {
      if(isWritingToPong)
        log("\t\t[Buffer0: DRAM is writing]")
      else
        log("\t\t[Buffer0: ARRAY is reading]")
    }

    if(pingBuffer.isEmpty){
      log("\t\tEmpty")
    } else {
      pingBuffer.foreach(tile => tile.printTile())
    }

    log("")
    if(dataType == DataType.A | dataType == DataType.B){
      if(isWritingToPong)
        log("\t\t[Buffer1: DRAM is writing]")
      else
        log("\t\t[Buffer1: ARRAY is reading]")
    } else if (dataType == DataType.C){
      if(isWritingToPong)
        log("\t\t[Buffer1: ARRAY is reading]")
      else
        log("\t\t[Buffer1: DRAM is writing]")
    }


    if(pongBuffer.isEmpty){
      log("\t\tEmpty")
    } else {
      pongBuffer.foreach(tile => tile.printTile())
    }
    log("")

  }

}

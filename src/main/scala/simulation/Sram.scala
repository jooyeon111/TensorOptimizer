package simulation

import simulation.DataType.DataType

import scala.collection.mutable

trait Sram extends Hardware with Logger {

  //Abstract members
  val dataType: DataType
  protected val singleBufferLimitKb: Int
  protected val singleBufferTileCapacity: Int

  //Memory configuration
  protected val sramTotalSizeBit: Double = singleBufferLimitKb.toDouble * 2.0 * 8.0 * 1024.0

  //Double buffer state
  private var bufferToggleCount: Int = 0
  private var isWritingToPong: Boolean = false
  protected var isFirstFillUpDone: Boolean = false

  //memory monitoring
  private var accumulatedMemoryUsageBit: Double = 0.0
  private var accumulatedMemoryUtilization: Double = 0.0

  //Buffers
  private val pingBuffer: mutable.Queue[Tile] = mutable.Queue.empty[Tile]
  private val pendingPingBuffer: mutable.Queue[Tile] = mutable.Queue.empty[Tile]
  private val pongBuffer: mutable.Queue[Tile] = mutable.Queue.empty[Tile]
  private val pendingPongBuffer: mutable.Queue[Tile] = mutable.Queue.empty[Tile]

  //Buffer access
  protected def readBuffer: mutable.Queue[Tile] = if(isWritingToPong) pingBuffer else pongBuffer
  protected def writeBuffer: mutable.Queue[Tile]= if(isWritingToPong) pongBuffer else pingBuffer
  protected def readPendingBuffer: mutable.Queue[Tile] = if(isWritingToPong) pendingPingBuffer else pendingPongBuffer
  protected def writePendingBuffer: mutable.Queue[Tile] = if(isWritingToPong) pendingPongBuffer else pendingPingBuffer

  //Buffer operations
  protected  def swapBuffers(): Unit = {
    isWritingToPong = !isWritingToPong
  }

  protected def isBufferIntactAndFull(buffer: mutable.Queue[Tile]): Boolean = {
    if (buffer.forall(tile => tile.ownedBySram)
      && (singleBufferTileCapacity - buffer.length == 0))
      true
    else
      false
  }

  //Memory monitoring
  protected def updateMemoryMonitor(): Unit = {
    accumulatedMemoryUsageBit += calculateMemoryUsage()
    accumulatedMemoryUtilization += calculateMemoryUtilization()
  }

  private def calculateMemoryUsage(): Double = {
    pingBuffer.map(_.dims.memorySize).sum +
      pongBuffer.map(_.dims.memorySize).sum
  }

  private def calculateMemoryUtilization(): Double = {
    calculateMemoryUsage() / sramTotalSizeBit * 100.0
  }

  //public API
  def getBufferToggleCount: Int = bufferToggleCount
  def increaseBufferToggleCount(): Unit = bufferToggleCount += 1
  def getAccumulatedMemoryUsage: Double = accumulatedMemoryUsageBit
  def getAccumulateMemoryUtilization: Double = accumulatedMemoryUtilization

  //Overriding from Hardware
  override def isHardwareEmpty: Boolean =
    readBuffer.isEmpty &&
      readPendingBuffer.isEmpty &&
      writeBuffer.isEmpty &&
      writePendingBuffer.isEmpty


  def printSram(): Unit = {
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

    if(isWritingToPong)
      log("\t\t[Buffer0: Writing to ARRAY]")
    else
      log("\t\t[Buffer0: Reading from DRAM]")


    if(pingBuffer.isEmpty){
      log("\t\tEmpty")
    } else {
      pingBuffer.foreach(tile => tile.printTile())
    }

    log("")
    if(isWritingToPong)
      log("\t\t[Buffer1: Reading from DRAM]")
    else
      log("\t\t[Buffer1: Writing to ARRAY]")

    if(pongBuffer.isEmpty){
      log("\t\tEmpty")
    } else {
      pongBuffer.foreach(tile => tile.printTile())
    }
    log("")

  }

}

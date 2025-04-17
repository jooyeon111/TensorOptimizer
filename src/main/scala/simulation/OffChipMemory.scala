package simulation

import common.Dataflow
import scala.collection.mutable
import simulation.DataType.DataType

final class OffChipMemory(
  val outputBandwidth: Int,
  val referenceData: Option[OffChipMemoryReferenceData] = None,
  val loggerOption: LoggerOption
) extends Hardware with Logger with AccessCounter {

  setMode(loggerOption)

  private val currentTileQueue: mutable.Queue[Tile] = mutable.Queue.empty[Tile]
  private val nextTileQueue: mutable.Queue[Tile] = mutable.Queue.empty[Tile]

  private val sendingTileQueueA: mutable.Queue[Tile] = mutable.Queue.empty[Tile]
  private val sendingTileQueueB: mutable.Queue[Tile] = mutable.Queue.empty[Tile]

  private var availableOutputBandwidth: Int = outputBandwidth
  private var temporalOutputBandwidth: Int = -1
  private var loopEscape: Boolean = true

  private var singleBufferTileCapacityOfSramA = -1
  private var singleBufferTileCapacityOfSramB = -1
//  private var writeEnable: Boolean = false

  //TODO change variable names below
  var skipTileCountA: Int = 0
  var skipTileCountB: Int = 0
  var offChipMemoryStall = 0

  //Function called by Compiler

  def getOffChipMemoryReadEnergy: Option[Double] =
    referenceData.map( data => getReadAccessCount * data.readEnergyPj )

  def getOffChipMemoryWriteEnergy: Option[Double] =
    referenceData.map( data => getWriteAccessCount * data.writeEnergyPj )

  def getOffChipMemoryEnergy: Option[Double] =
    referenceData.map{ data =>
      getReadAccessCount * data.readEnergyPj +
        getWriteAccessCount * data.writeEnergyPj
    }

  //TODO assert this function is called once
  private def uploadInitialTiles(operationVector: Vector[MultiplicationOperation], dataflow: Dataflow.Value) : Unit = {
    dataflow match {

      case Dataflow.Is =>
        operationVector.foreach{ operation =>
          currentTileQueue += operation.generateTileA
          currentTileQueue += operation.generateTileB
        }
      case Dataflow.Ws =>
        operationVector.foreach{ operation =>
          currentTileQueue += operation.generateTileB
          currentTileQueue += operation.generateTileA
        }
      case Dataflow.Os =>
        operationVector.foreach{ operation =>
          currentTileQueue += operation.generateTileA
          currentTileQueue += operation.generateTileB
        }
    }

  }

  def getTileIdsFromCurrentTileQueue(dataType: DataType): Set[(Int, Int)] = {

    val currentTiles = dataType match {
      case DataType.A =>
        currentTileQueue
          .filter(tile => tile.dataType == DataType.A)
      case DataType.B =>
        currentTileQueue
          .filter(tile => tile.dataType == DataType.A)
      case _ =>
        Console.err.println(s"[error] Invalid data type for Off Chip Memory")
        sys.exit(1)
    }

    currentTiles
      .map(_.id.asInstanceOf[(Int,Int)])
      .toSet

  }

  def initOffChipMemory(operationVector: Vector[MultiplicationOperation], dataflow: Dataflow.Value) : Unit = {
    uploadInitialTiles(operationVector, dataflow)
  }

  //Function called by array
  def receive(): Unit = {
    incrementWriteAccessCount()
  }

  override def update(interface: Interface) : Unit = {

    if(currentTileQueue.nonEmpty && isReadyToSend){

      prepareTileInTransit()

      singleBufferTileCapacityOfSramA = interface.sramA.howManyTileCanStore()
      singleBufferTileCapacityOfSramB = interface.sramB.howManyTileCanStore()

      while(availableOutputBandwidth > 0 && ((singleBufferTileCapacityOfSramA > 0) || (singleBufferTileCapacityOfSramB > 0)) && loopEscape){

        if(currentTileQueue.nonEmpty)
          trim(interface)

        temporalOutputBandwidth = availableOutputBandwidth

        if(currentTileQueue.nonEmpty)
          prepareTileForSend()

        if(temporalOutputBandwidth == availableOutputBandwidth)
          loopEscape = false

      }

      send(interface)

      loopEscape = true
      availableOutputBandwidth = outputBandwidth
      temporalOutputBandwidth = -1
      singleBufferTileCapacityOfSramA = -1
      singleBufferTileCapacityOfSramB = -1

    }

    updateState()

  }

  override def updateState(): Unit =
    while(nextTileQueue.nonEmpty) {
      if(currentTileQueue.isEmpty){
        currentTileQueue.enqueue(nextTileQueue.dequeue())
      } else {
        if(currentTileQueue.last == nextTileQueue.front && !currentTileQueue.last.ownedByOffChipMemory){
          currentTileQueue.removeLast()
          currentTileQueue.enqueue(nextTileQueue.dequeue())
        } else {
          currentTileQueue.enqueue(nextTileQueue.dequeue())
        }
      }
    }

  private def prepareTileInTransit(): Unit = {

    val tile = currentTileQueue.front

    if(tile.memoryOccupiedBySram > 0){

      if(availableOutputBandwidth - tile.memoryOccupiedByOffChipMemory >= 0){
        availableOutputBandwidth = availableOutputBandwidth - tile.memoryOccupiedByOffChipMemory
        tile.memoryOccupiedBySram = tile.dims.memorySize
        tile.memoryOccupiedByOffChipMemory = 0

        tile.dataType match {
          case DataType.A => sendingTileQueueA.enqueue(currentTileQueue.dequeue())
          case DataType.B => sendingTileQueueB.enqueue(currentTileQueue.dequeue())
          case _ =>
            Console.err.println("[error] OffChipMemory cannot handle this type of tile")
            sys.exit(1)
        }

      } else {
        tile.memoryOccupiedBySram += availableOutputBandwidth
        tile.memoryOccupiedByOffChipMemory = tile.dims.memorySize - tile.memoryOccupiedBySram
        availableOutputBandwidth = 0

        tile.dataType match {
          case DataType.A => sendingTileQueueA.enqueue(tile.copyTile())
          case DataType.B => sendingTileQueueB.enqueue(tile.copyTile())
          case _ =>
            Console.err.println("[error] OffChipMemory cannot handle this type of tile")
            sys.exit(1)
        }

      }
    }

  }

  private def trim(interface: Interface): Unit = {
    def shouldTrimTile(tile: Tile): Boolean = {
      if (!tile.ownedByOffChipMemory) return false

      tile.dataType match {
        case DataType.A =>
          interface.sramA.existsInInBuffers(tile) || sendingTileQueueA.exists(_.id == tile.id)
        case DataType.B =>
          interface.sramB.existsInInBuffers(tile) || sendingTileQueueB.exists(_.id == tile.id)
        case _ =>
          Console.err.println("[error] Invalid data type in OffChipMemory Tile Queue")
          sys.exit(1)
      }
    }

    def processTile(tile: Tile): Unit = {
      tile.dataType match {
        case DataType.A =>
          skipTileCountA += 1
        case DataType.B =>
          skipTileCountB += 1
        case _ =>
          Console.err.println("[error] Invalid data type in OffChipMemory Tile Queue")
          sys.exit(1)
      }
      currentTileQueue.dequeue()
    }

    while (currentTileQueue.nonEmpty && shouldTrimTile(currentTileQueue.front)) {
      processTile(currentTileQueue.front)
    }

  }

  override def prepareTileForSend(): Unit = {
    val tile = currentTileQueue.front

    def processTile(sramCapacity: Int, sendingQueue: mutable.Queue[Tile]): Unit = {
      if (sramCapacity > 0) {
        val remainingBandwidth = availableOutputBandwidth - tile.memoryOccupiedByOffChipMemory

        if (remainingBandwidth >= 0) {
          availableOutputBandwidth -= tile.memoryOccupiedByOffChipMemory
          tile.memoryOccupiedBySram = tile.dims.memorySize
          tile.memoryOccupiedByOffChipMemory = 0
          sendingQueue.enqueue(currentTileQueue.dequeue())
        } else {
          tile.memoryOccupiedBySram += availableOutputBandwidth
          tile.memoryOccupiedByOffChipMemory = tile.dims.memorySize - tile.memoryOccupiedBySram
          availableOutputBandwidth = 0
          sendingQueue.enqueue(tile.copyTile())
        }
      }
    }

    tile.dataType match {
      case DataType.A =>
        processTile(singleBufferTileCapacityOfSramA, sendingTileQueueA)
        singleBufferTileCapacityOfSramA -= 1
      case DataType.B =>
        processTile(singleBufferTileCapacityOfSramB, sendingTileQueueB)
        singleBufferTileCapacityOfSramB -= 1
      case _ =>
        Console.err.println("[error] Off chip memory cannot handle this type of tile")
        sys.exit(1)
    }
  }



  override def send(interface: Interface) : Unit = {

    if(sendingTileQueueA.isEmpty && sendingTileQueueB.isEmpty) {
      markTileSendFailed()
      offChipMemoryStall += 1
    } else {
      markTileSendSuccessful()
      incrementReadAccessCount()
    }

    if(sendingTileQueueA.nonEmpty)
      interface.sramA.receive(sendingTileQueueA)

    if(sendingTileQueueB.nonEmpty)
      interface.sramB.receive(sendingTileQueueB)

  }

  //Util functions
  def printTiles(): Unit = {
    log("\t[Off Chip Memory]")
    if(isHardwareEmpty){
      log("\tEmpty\n")
    } else {
      currentTileQueue.foreach(tile => tile.printTile())
    }
  }

  override def isHardwareEmpty: Boolean =
    currentTileQueue.isEmpty && nextTileQueue.isEmpty

}

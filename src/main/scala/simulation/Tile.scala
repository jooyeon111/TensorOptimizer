package simulation

import simulation.DataType.DataType

trait Tile extends Logger {

  val dataType: DataType
  val layerName: String
  val id : Any
  val dims: MatrixDimension
  final var memoryOccupiedBySram: Int = 0
  final var memoryOccupiedByDram: Int = 0
  final var memoryOccupiedByArray: Int = 0
  final var memoryCalculatedByArray: Int = 0
  final protected var state: TileState.Value = TileState.waiting
  require(layerName.nonEmpty, "[error] Empty layer name is not allowed")

  final def ownedBySram: Boolean = {
    if(memoryOccupiedBySram == dims.memorySize) {
      assert(memoryOccupiedByDram == 0, "[error] Wrong data movement")
      assert(memoryOccupiedByArray == 0, "[error] Wrong data movement")
      assert(memoryCalculatedByArray == 0, "[error] Wrong data movement")
      true
    } else
      false
  }

  final def ownedByDram: Boolean = {
    if(memoryOccupiedByDram == dims.memorySize){
      assert(memoryOccupiedBySram == 0, "[error] Wrong data movement")
      assert(memoryOccupiedByArray == 0, "[error] Wrong data movement")
      assert(memoryCalculatedByArray == 0, "[error] Wrong data movement")
      true
    } else
      false
  }

  final def ownedByArray: Boolean = {

    if((memoryOccupiedByArray + memoryCalculatedByArray) == dims.memorySize){
      assert(memoryOccupiedByDram == 0, "[error] Wrong data movement")
      assert(memoryOccupiedBySram == 0, "[error] Wrong data movement")
      true
    } else
      false

  }

  final def reallocateToSram(): Unit = {

    assert(memoryOccupiedByArray == dims.memorySize)
    assert(memoryOccupiedBySram == 0)
    assert(memoryOccupiedByDram == 0)
    assert(memoryCalculatedByArray == 0)

    memoryOccupiedByArray = 0
    memoryOccupiedBySram = dims.memorySize
    memoryOccupiedByDram = 0
    memoryCalculatedByArray = 0

    state = TileState.waiting

  }

  type T <: Tile
  def copyTile(): T

  final def copyDataTransferData(that: Tile): Unit = {

    this.memoryOccupiedBySram = that.memoryOccupiedBySram
    this.memoryOccupiedByDram = that.memoryOccupiedByDram
    this.memoryOccupiedByArray = that.memoryOccupiedByArray - this.memoryCalculatedByArray

  }

  final def totalMemoryUsedByArray: Int =
    memoryOccupiedByArray + memoryCalculatedByArray

  final def startCalculation(): Unit = {
    state = TileState.calculating
  }

  final def completeCalculation(): Unit = {
    state = TileState.calculated
  }

  final def isWaiting: Boolean = {
    state == TileState.waiting
  }

  final def isCalculating: Boolean = {
    state == TileState.calculating
  }

  final def isCalculated: Boolean = {
    state == TileState.calculated
  }

  def startLoading(): Unit = {
    state = TileState.loading
  }

  def completeLoading(): Unit = {
    state = TileState.loaded
  }

  def isLoading: Boolean = {
    state == TileState.loading
  }

  def isLoaded: Boolean = {
    state == TileState.loaded
  }

  final def identifyState: TileState.Value = state

  def printTile() : Unit = {

    log(s"\t\tDataType: $dataType ID: $id")
    dims.printDimension()
    log(s"\t\tDRAM: $memoryOccupiedByDram, SRAM: $memoryOccupiedBySram, Array: $memoryOccupiedByArray, Calculated: $memoryCalculatedByArray")
    logWithoutNewLine("\t\tState: ")
    state match {
      case TileState.waiting =>
        log("waiting")
      case TileState.loading =>
        log("loading")
      case TileState.loaded =>
        log("loaded")
      case TileState.calculating =>
        log("calculating")
      case TileState.calculated =>
        log("calculated")
      case _ =>
        Console.err.println(s"Wrong tile state")
        sys.exit(1)

    }
    log("")

  }

}

final class TileA(
  override val layerName: String ,
  override val id : (Int, Int),
  override val dims: MatrixDimension,
  val loggerOption: LoggerOption
) extends Tile {

  setMode(loggerOption)

  override val dataType: DataType = DataType.A
  memoryOccupiedByDram = dims.memorySize
  assert(dataType == dims.dataType, s"[error] Tile id = $id data type and matrix dimension type do not match")

  type T = TileA
  override def copyTile(): T = {
    val copiedTile = new TileA(
      layerName = this.layerName,
      id = this.id,
      dims = this.dims,
      loggerOption = this.loggerOption,
    )
    copiedTile.memoryOccupiedByDram = this.memoryOccupiedByDram
    copiedTile.memoryOccupiedByArray = this.memoryOccupiedByArray
    copiedTile.memoryOccupiedBySram = this.memoryOccupiedBySram
    copiedTile.state = this.state
    copiedTile
  }


}

final class TileB(
  override val layerName: String ,
  override val id : (Int, Int),
  override val dims: MatrixDimension,
  val loggerOption: LoggerOption
) extends Tile {

  setMode(loggerOption)

  override val dataType: DataType = DataType.B
  memoryOccupiedByDram = dims.memorySize
  require(dataType == dims.dataType, s"[error] Tile id = $id data type and matrix dimension type do not match")

  type T = TileB
  override def copyTile(): T = {
    val copiedTile = new TileB(
      layerName = this.layerName,
      id = this.id,
      dims = this.dims,
      loggerOption = this.loggerOption
    )
    copiedTile.memoryOccupiedByDram = this.memoryOccupiedByDram
    copiedTile.memoryOccupiedByArray = this.memoryOccupiedByArray
    copiedTile.memoryOccupiedBySram = this.memoryOccupiedBySram
    copiedTile.state = this.state
    copiedTile
  }


}


final class TileC(
  override val layerName: String ,
  override val id : (Int, Int, Int),
  override val dims: MatrixDimension,
  val loggerOption: LoggerOption
) extends Tile {

  setMode(loggerOption)

  override val dataType: DataType = DataType.C
  memoryOccupiedByArray = dims.memorySize
  require(dataType == dims.dataType, s"[error] Tile id = $id data type and matrix dimension type do not match")

  type T = TileC
  override def copyTile(): T = {
    val copiedTile = new TileC(
      layerName = this.layerName,
      id = this.id,
      dims = this.dims,
      loggerOption = loggerOption
    )
    copiedTile.memoryOccupiedByDram = this.memoryOccupiedByDram
    copiedTile.memoryOccupiedByArray = this.memoryOccupiedByArray
    copiedTile.memoryOccupiedBySram = this.memoryOccupiedBySram
    copiedTile.state = this.state
    copiedTile.mode = this.mode
    copiedTile.outputFile = this.outputFile
    copiedTile
  }

  def willCalculateTile(): Unit =
    state = TileState.nextCalculation

  def isNextCalculation: Boolean =
    state == TileState.nextCalculation

  override def printTile(): Unit = {
    log(s"\t\tLayer Name: $layerName DataType: $dataType ID: $id")
    dims.printDimension()
    log(s"\t\tDRAM: $memoryOccupiedByDram SRAM: $memoryOccupiedBySram Array: $memoryOccupiedByArray Calculated: $memoryCalculatedByArray")

    logWithoutNewLine("\t\tState: ")
    state match {
      case TileState.waiting =>
        log("waiting")
      case TileState.loading =>
        log("loading")
      case TileState.loaded =>
        log("loaded")
      case TileState.calculating =>
        log("calculating")
      case TileState.calculated =>
        log("calculated")
      case TileState.nextCalculation =>
        log("Will receive")
      case _ =>
        Console.err.println(s"[error] Wrong tile state")
        sys.exit(1)
    }

    log("")

  }
}

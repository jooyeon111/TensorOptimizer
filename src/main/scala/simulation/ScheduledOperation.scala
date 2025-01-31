package simulation

import common.Dataflow
import scala.math.{ceil, log10}

class ScheduledOperation(
  val dataflow: Dataflow.Value,
  val layerName: String,
  val operationId:(Int, Int, Int),
  private val tileC: TileC,
  val isTileAUsedInNextOp: Boolean,
  val isTileBUsedInNextOp: Boolean,
  val loggerOption: LoggerOption
) extends Logger {

  setMode(loggerOption)

  private var tileA: TileA = _
  private var tileB: TileB = _

  private var inputATileGoneTimer: Int = -1
  private var inputBTileGoneTimer: Int = - 1
  private var outputTileGenerationTimer: Int = -1

  //Output outputTileGenerationTimer setting
  def updateTimer(): Unit = {
    require(inputATileGoneTimer != -1, s"[error] Input tile gone timer is not set Input Timer A: $inputATileGoneTimer")
    require(inputBTileGoneTimer != -1 , s"[error] Input tile gone timer is not set Input Timer B: $inputBTileGoneTimer")
    require(outputTileGenerationTimer != -1, "[error] Output tile generation timer is not set")

    inputATileGoneTimer -= 1
    if(inputATileGoneTimer < 0){
      inputATileGoneTimer = 0
    }

    inputBTileGoneTimer -= 1
    if(inputBTileGoneTimer < 0){
      inputBTileGoneTimer = 0
    }

    outputTileGenerationTimer -= 1
    if(outputTileGenerationTimer < 0){
      outputTileGenerationTimer = 0
    }
  }

  def isInputATileGoneTimerExpired: Boolean = {

    dataflow match {
      case Dataflow.Is =>
        tileB.totalMemoryUsedByArray == tileB.dims.memorySize

      case Dataflow.Os =>
        inputATileGoneTimer == 0

      case Dataflow.Ws =>
        inputATileGoneTimer == 0

      case _=>
        Console.err.println(s"[error] Invalid dataflow")
        sys.exit(1)
    }


  }

  def isInputBTileGoneTimerExpired: Boolean = {

    dataflow match {
      case Dataflow.Is =>
        inputBTileGoneTimer == 0

      case Dataflow.Os =>
        inputBTileGoneTimer == 0

      case Dataflow.Ws =>
        tileA.totalMemoryUsedByArray == tileA.dims.memorySize

      case _=>
        Console.err.println(s"[error] Invalid dataflow")
        sys.exit(1)
    }


  }

  def isOutputTileGenerationTimerExpired: Boolean =
    outputTileGenerationTimer == 0

  //Set and get
  def setTileA(tileA: TileA): Unit = {
    assert(tileA.id == getTileAId, s"[error] tileA id and operation id dose not match $operationId required tile A ID: ${getTileAId} received tile A ID: ${tileA.id} Layer: $layerName")
    this.tileA = tileA
  }

  def setTileB(tileB: TileB): Unit = {
    assert(tileB.id == getTileBId, s"[error] tileB id and operation id dose not match $operationId required tile A ID: ${getTileBId} received tile A ID: ${tileB.id} Layer: $layerName")
    this.tileB = tileB
  }

  def getTileA: TileA = tileA

  def getTileB: TileB = tileB

  def getTileC: TileC = tileC

  def getTileAId: (Int, Int) = (operationId._1, operationId._3)

  def getTileBId: (Int, Int) = (operationId._3, operationId._2)

def getRequiredTileAId: (Int, Int) = {
  def needMemorySpace(tile: Tile): Boolean =
    tile.totalMemoryUsedByArray < tile.dims.memorySize

  dataflow match {
    case Dataflow.Is =>
      if (isNextCalculation) getTileAId
      else if (isLoading && needMemorySpace(tileA)) tileA.id
      else (-1, -1)

    case Dataflow.Os =>
      require(!isLoading && !isCalculated, "[error] output stationary invalid state")
      if (isNextCalculation) getTileAId
      else if (isCalculating && needMemorySpace(tileA)) tileA.id
      else (-1, -1)

    case Dataflow.Ws =>
      if (isLoaded) getTileAId
      else if (isCalculating && needMemorySpace(tileA)) getTileAId
      else (-1, -1)

    case _ =>
      Console.err.println(s"[error] Invalid dataflow: $dataflow")
      sys.exit(1)
  }
}

  def getRequiredTileBId: (Int, Int) = {
    def needMemorySpace(tile: Tile): Boolean =
      tile.totalMemoryUsedByArray < tile.dims.memorySize

    dataflow match {
      case Dataflow.Is =>
        if (isLoaded) getTileBId
        else if (isCalculating && needMemorySpace(tileB)) getTileBId
        else (-1, -1)

      case Dataflow.Os =>
        require(!isLoading && !isLoaded && !isCalculated, "[error] output stationary invalid state")
        if (isNextCalculation) getTileBId
        else if (isCalculating && needMemorySpace(tileB)) tileB.id
        else (-1, -1)

      case Dataflow.Ws =>
        if (isNextCalculation) getTileBId
        else if (isLoading && needMemorySpace(tileB)) tileB.id
        else (-1, -1)

      case _ =>
        Console.err.println(s"[error] Invalid dataflow: $dataflow")
        sys.exit(1)
    }
  }

  //State check
  def isWaiting: Boolean =
    tileC.isWaiting

  def isNextCalculation: Boolean =
    tileC.isNextCalculation

  def isLoading: Boolean =
    tileC.isLoading

  def isLoaded: Boolean =
    tileC.isLoaded

  def isCalculating: Boolean =
    tileC.isCalculating

  def isCalculated: Boolean =
    tileC.isCalculated

  def needTile: Boolean = {
    def needInputTile(tile: Tile): Boolean =
      tile.totalMemoryUsedByArray < tile.dims.memorySize

    dataflow match {
      case Dataflow.Is =>
        isNextCalculation ||
          (isLoading && needInputTile(tileA)) ||
          isLoaded ||
          (isCalculating && needInputTile(tileB))

      case Dataflow.Os =>
        require(!isLoading && !isLoaded, "[error] output stationary cannot have loading states")
        isNextCalculation ||
          (isCalculating && (needInputTile(tileA) || needInputTile(tileB)))

      case Dataflow.Ws =>
        isNextCalculation ||
          (isLoading && needInputTile(tileB)) ||
          isLoaded ||
          (isCalculating && needInputTile(tileA))
    }

  }

  //State convert
  def startLoading(): Unit = {
    require(dataflow != Dataflow.Os, "[error] Output stationary systolic tensor array cannot call this function")
    dataflow match {
      case Dataflow.Is =>
        tileA.startLoading()
        tileC.startLoading()
      case Dataflow.Ws =>
        tileB.startLoading()
        tileC.startLoading()
      case _ =>
        Console.err.println(
          s"[error] This type of dataflow cannot use start loading function Current Dataflow: $dataflow")
        sys.exit(1)
    }
  }

  def completeLoading(): Unit = {
    require(dataflow != Dataflow.Os, "[error] Output stationary systolic tensor array cannot call this function")
    dataflow match {
      case Dataflow.Is =>
        tileA.completeLoading()
        tileC.completeLoading()
      case Dataflow.Ws =>
        tileB.completeLoading()
        tileC.completeLoading()
      case _ =>
        Console.err.println(
          s"[error] This type of dataflow cannot use complete loading Current Dataflow: $dataflow")
        sys.exit(1)
    }
  }

  //TODO split function
  def startCalculation(arrayConfig: ArrayConfig): Unit = {
    //Is or Ws
    //array col * block col or array row * block row: skew buffer(pre processor)
    //ceil(log10(arrayConfig.multiplierPerPe)/log10(2.0)).toInt: Adder tree height
    //1: one register inside ofPE
    //1: block PE register
    //array row or array col: deskew buffer(post processor)

    //OS
    //tiling size
    //ceil(log10(arrayConfig.multiplierPerPe)/log10(2.0)).toInt: Adder tree height
    //2: two register inside of PE
    //1: block PE register
    //array config.array - 1: deskew buffer stage
    //1: railway module register

    val peBasicDelay = 2 + ceil(log10(arrayConfig.numMultiplier)/log10(2.0)).toInt

    arrayConfig.dataflow match {
      case Dataflow.Is =>

        if(!isTileAUsedInNextOp)
          tileA.startCalculation()

        tileB.startCalculation()
        tileC.startCalculation()

        inputATileGoneTimer = 0
        if(inputBTileGoneTimer == -1)
          inputBTileGoneTimer = peBasicDelay
        outputTileGenerationTimer = (arrayConfig.groupPeCol * arrayConfig.vectorPeCol) + peBasicDelay + 1 + arrayConfig.groupPeRow

      case Dataflow.Os =>

        assert(!isLoading, "Output stationary cannot have this state")
        assert(!isLoaded, "Output stationary cannot have this state" )
        assert(!isCalculated, "Right now this stat cannot happen")
        assert(tileA.dims.col == tileB.dims.row, s"Tile A and Tile B dimension is wrong")
        assert(tileA.dims.memorySize / arrayConfig.bandwidthOfInputA == tileB.dims.memorySize / arrayConfig.bandwidthOfInputB,
          s"Tile A and Tile B dimension is wrong")

        tileA.startCalculation()
        tileB.startCalculation()
        tileC.startCalculation()

        inputATileGoneTimer = peBasicDelay
        inputBTileGoneTimer = peBasicDelay
        outputTileGenerationTimer = tileA.dims.memorySize / arrayConfig.bandwidthOfInputA + peBasicDelay + 1 + (arrayConfig.groupPeRow - 1) + 1

        assert(outputTileGenerationTimer > inputATileGoneTimer, "[error] Input gone timer is must be faster than output tile generation timer")
        assert(outputTileGenerationTimer > inputBTileGoneTimer, "[error] Input gone timer is must be faster than output tile generation timer")
        assert(inputATileGoneTimer == inputBTileGoneTimer, "[error] input A perish timer and weight B perish timer must be same")

      case Dataflow.Ws =>

        tileA.startCalculation()
        if(!isTileBUsedInNextOp)
          tileB.startCalculation()

        tileC.startCalculation()

        inputATileGoneTimer = peBasicDelay
        inputBTileGoneTimer = 0
        outputTileGenerationTimer = (arrayConfig.groupPeRow * arrayConfig.vectorPeRow) + peBasicDelay + 1 + arrayConfig.groupPeCol

//        assert()

      case  _ =>
        Console.err.println(s"[error] Invalid dataflow")
        sys.exit(1)
    }

  }

  def completeCalculation(): Unit = {
    assert(isCalculating, "[error] Only calculating operation can enter complete calculation function")
    tileA.completeCalculation()
    tileB.completeCalculation()
    tileC.completeCalculation()
  }

  def willCalculateTile(): Unit =
    tileC.willCalculateTile()

  //Util
  def printSchedule() : Unit = {

    tileC.identifyState match {
      case TileState.waiting | TileState.calculating | TileState.loading | TileState.loaded | TileState.calculated | TileState.nextCalculation =>
//        logWithoutNewLine(s"\t\tLayer Name: $layerName ID: $operationId State: ${tileC.identifyState} ")
        if(isTileAUsedInNextOp)
          log(s"\t\tLayer Name: $layerName ID: $operationId State: ${tileC.identifyState} TileA Used in Next Op: $isTileAUsedInNextOp")
        else if(isTileBUsedInNextOp)
          log(s"\t\tLayer Name: $layerName ID: $operationId State: ${tileC.identifyState} TileB Used in Next Op: $isTileBUsedInNextOp")
        else
          log(s"\t\tLayer Name: $layerName ID: $operationId State: ${tileC.identifyState}")

//      case TileState.waiting =>
      case _ =>

    }

  }

}

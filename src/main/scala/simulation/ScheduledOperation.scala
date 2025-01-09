package simulation

import common.Dataflow
import common.Dataflow.Dataflow

import scala.math.{ceil, log10}

case class ScheduledOperation(
  layerName: String,
  operationId:(Int, Int, Int),
  private val tileC: TileC,
  isTileAUsedInNextOp: Boolean,
  isTileBUsedInNextOp: Boolean,
  loggerOption: LoggerOption
) extends Logger {

  setMode(loggerOption)

  private var tileA: TileA = _
  private var tileB: TileB = _

  private var inputATileGoneTimer: Int = -1
  private var inputBTileGoneTimer: Int = - 1
  private var outputTileGenerationTimer: Int = -1

  //Output outputTileGenerationTimer setting
  def updateTimer(): Unit = {
    require(inputATileGoneTimer != -1, s"[error] Input tile gone timer is not set Input Timer: $inputATileGoneTimer")
    require(inputBTileGoneTimer != -1 , s"[error] Input tile gone timer is not set Input Timer: $inputBTileGoneTimer")
    require(outputTileGenerationTimer != -1, "[error] Output tile generation timer is not set")

    inputATileGoneTimer -= 1
    if(inputATileGoneTimer < 0){
      inputATileGoneTimer = 0
    }

    inputBTileGoneTimer -= 1
    if(inputBTileGoneTimer <0){
      inputBTileGoneTimer = 0
    }

    outputTileGenerationTimer -= 1
    if(outputTileGenerationTimer < 0){
      outputTileGenerationTimer = 0
    }
  }

  def isInputATileGoneTimerExpired(dataflow: Dataflow): Boolean = {

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

  def isInputBTileGoneTimerExpired(dataflow: Dataflow): Boolean = {

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
    assert(tileA.id == getTileAId, s"[error] tileA id and operation id dose not match $operationId Layer: $layerName")
    this.tileA = tileA
  }

  def setTileB(tileB: TileB): Unit = {
    assert(tileB.id == getTileBId, s"[error] tileB id and operation id dose not match $operationId Layer: $layerName")
    this.tileB = tileB
  }

  def getTileA: TileA = tileA

  def getTileB: TileB = tileB

  def getTileC: TileC = tileC

  def getTileAId: (Int, Int) = (operationId._1, operationId._3)

  def getTileBId: (Int, Int) = (operationId._3, operationId._2)

  def getRequiredTileAId(dataflow: Dataflow): (Int, Int) =
    dataflow match {
      case Dataflow.Is =>

      if(isNextCalculation){
        getTileAId
      } else if (isLoading){
        if(tileA.totalMemoryUsedByArray < tileA.dims.memorySize)
          tileA.id
        else
          (-1, -1)

      } else
        (-1, -1)

      case Dataflow.Os =>
        assert(!isLoading, "[error] output stationary cannot have this state")
        assert(!isCalculated, "[error] right now this stat cannot happen")

        if(isNextCalculation){
          getTileAId
        } else if(isCalculating){
          if(tileA.totalMemoryUsedByArray < tileA.dims.memorySize)
            tileA.id
          else
            (-1, -1)
        } else {
          Console.err.println(s"This state dose not exist")
          sys.exit(1)
        }


      case Dataflow.Ws =>

        if(isLoaded){
          getTileAId
        } else if (isCalculating){
          if(tileA.totalMemoryUsedByArray < tileA.dims.memorySize)
            getTileAId
          else
            (-1, -1)
        } else
          (-1, -1)

      case _ =>
        Console.err.println(
          s"[error] This type of dataflow cannot use start loading function Current Dataflow: $dataflow")
        sys.exit(1)
    }


  def getRequiredTileBId(dataflow: Dataflow): (Int, Int) =
    dataflow match {
      case Dataflow.Is =>
//        if(tileA.totalMemoryUsedByArray >= tileA.dims.memorySize){
//          tileB.id
//        } else
//          (-1,-1)

      if(isLoaded){
        getTileBId
      } else if(isCalculating){

        if(tileB.totalMemoryUsedByArray < tileB.dims.memorySize)
          getTileBId
        else
          (-1, -1)

      } else
        (-1, -1)

      case Dataflow.Os =>
        assert(!isLoading, "[error] output stationary cannot have this state")
        assert(!isLoaded, "[error] output stationary cannot have this state")
        assert(!isCalculated, "[error] right now this stat cannot happen")

        if(isNextCalculation){
          getTileBId
        } else if (isCalculating){
          if(tileB.totalMemoryUsedByArray < tileB.dims.memorySize)
            tileB.id
          else
            (-1, -1)
        } else {
          Console.err.println(s"This state dose not exist")
          sys.exit(1)
        }

      case Dataflow.Ws =>

        if(isNextCalculation){
          getTileBId
        } else if (isLoading){

          if(tileB.totalMemoryUsedByArray < tileB.dims.memorySize)
            tileB.id
          else
            (-1, -1)

        } else {
          (-1, -1)
        }

      case _ =>
        Console.err.println(
          s"[error] This type of dataflow cannot use start loading function Current Dataflow: $dataflow")
        sys.exit(1)
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

  def needTile(dataflow: Dataflow): Boolean = {

    dataflow match {
      case Dataflow.Is =>
        if(isNextCalculation){
          true
        } else if(isLoading){

          if(tileA.totalMemoryUsedByArray < tileA.dims.memorySize)
            true
          else
            false

        } else if(isLoaded){
          true
        } else if(isCalculating){

          if(tileB.totalMemoryUsedByArray < tileB.dims.memorySize)
            true
          else
            false

        } else {
          false
        }


      case Dataflow.Os =>
        assert(!isLoading, "[error] output stationary cannot have this state")
        assert(!isLoaded, "[error] output stationary cannot have this state")

        if(isNextCalculation) {
          true
        } else if(isCalculating) {

          if(tileA.totalMemoryUsedByArray < tileA.dims.memorySize || tileB.totalMemoryUsedByArray < tileB.dims.memorySize)
            true
          else
            false

        } else {
          false
        }

      case Dataflow.Ws =>

        if(isNextCalculation){
          true
        } else if(isLoading){

          if(tileB.totalMemoryUsedByArray < tileB.dims.memorySize)
            true
          else
            false

        } else if(isLoaded){
          true
        } else if(isCalculating){

          if(tileA.totalMemoryUsedByArray < tileA.dims.memorySize)
            true
          else
            false

        } else {
          false
        }

    }
  }

  //State convert
  def startLoading(dataflow: Dataflow): Unit = {
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

  def completeLoading(dataflow: Dataflow): Unit = {
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

    tileA.startCalculation()
    tileB.startCalculation()
    tileC.startCalculation()

    val peBasicDelay = 2 + ceil(log10(arrayConfig.numMultiplier)/log10(2.0)).toInt

    arrayConfig.dataflow match {
      case Dataflow.Is =>

        inputATileGoneTimer = 0
        inputBTileGoneTimer = peBasicDelay
        outputTileGenerationTimer = (arrayConfig.groupPeCol * arrayConfig.vectorPeCol) + peBasicDelay + 1 + arrayConfig.groupPeRow

      case Dataflow.Os =>

        assert(!isLoading, "Output stationary cannot have this state")
        assert(!isLoaded, "Output stationary cannot have this state" )
        assert(!isCalculated, "Right now this stat cannot happen")
        assert(tileA.dims.col == tileB.dims.row, s"Tile A and Tile B dimension is wrong")
        assert(tileA.dims.memorySize / arrayConfig.bandwidthOfInputA == tileB.dims.memorySize / arrayConfig.bandwidthOfInputB,
          s"Tile A and Tile B dimension is wrong")


        inputATileGoneTimer = peBasicDelay
        inputBTileGoneTimer = peBasicDelay
        outputTileGenerationTimer = tileA.dims.memorySize / arrayConfig.bandwidthOfInputA + peBasicDelay + 1 + (arrayConfig.groupPeRow - 1) + 1

        assert(outputTileGenerationTimer > inputATileGoneTimer, "[error] Input gone timer is must be faster than output tile generation timer")
        assert(outputTileGenerationTimer > inputBTileGoneTimer, "[error] Input gone timer is must be faster than output tile generation timer")
        assert(inputATileGoneTimer == inputBTileGoneTimer, "[error] input A perish timer and weight B perish timer must be same")

      case Dataflow.Ws =>

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
  def printOperation() : Unit = {

//    tileC.identifyState match {
//      case TileState.calculating | TileState.loading | TileState.loaded | TileState.calculated | TileState.nextCalculation =>
//        log(s"\t\tLayer Name: $layerName ID: $operationId State: ${tileC.identifyState}")
//      case TileState.waiting =>
//
//    }

    log(s"\t\tLayer Name: $layerName ID: $operationId State: ${tileC.identifyState}")
//    log()
//    log(s"Tile A ID: $getTileAId, Tile B ID: $getTileBId")
//    log(s"Tile A used in nex operation: $isTileAUsedInNextOp")
//    log(s"Tile B used in nex operation: $isTileBUsedInNextOp")
//    log(s"TileC: ${tileC.identifyState}")
//    log("")
  }

}

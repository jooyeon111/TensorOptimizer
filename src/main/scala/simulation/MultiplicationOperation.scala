package simulation

import common.Dataflow

class MultiplicationOperation(
  val layerName: String,
  val operationId: (Int, Int, Int),
  val gemmDimension: GemmDimension,
  val arrayConfig: ArrayConfig,
  val loggerOption: LoggerOption
) extends Logger {

  setMode(loggerOption)

  def getTileAId: (Int, Int) = (operationId._1, operationId._3)
  def getTileBId: (Int, Int) = (operationId._3, operationId._2)
  private def getTileCId: (Int, Int, Int) = (operationId._1, operationId._2, operationId._3)

  def generateTileA : TileA = {

    val arrayRowDimension = arrayConfig.groupPeRow * arrayConfig.vectorPeRow * arrayConfig.numMultiplier
    val originalDimensionSize = gemmDimension.m * gemmDimension.k

    val matrixRow = arrayConfig.dataflow match {
      case Dataflow.Is | Dataflow.Ws =>
        gemmDimension.m

      case Dataflow.Os =>
        if( originalDimensionSize % arrayRowDimension == 0)
          gemmDimension.m
        else
          arrayRowDimension

      case _ =>
        Console.err.println(s"[error] Invalid dataflow")
        sys.exit(1)
    }

    val matrixCol = arrayConfig.dataflow match {
      case Dataflow.Is | Dataflow.Ws =>
        gemmDimension.k

      case Dataflow.Os =>
        if( originalDimensionSize % arrayRowDimension  == 0)
          gemmDimension.k
        else
          originalDimensionSize / arrayRowDimension + 1

      case _ =>
        Console.err.println(s"[error] Invalid dataflow")
        sys.exit(1)
    }

    new TileA(
      layerName = layerName,
      id = getTileAId,
      MatrixDimension(DataType.A, arrayConfig.portBitWidth.typeA, matrixRow, matrixCol, loggerOption),
      loggerOption
    )

  }

  def generateTileB : TileB = {

    val arrayColDimension = arrayConfig.groupPeCol * arrayConfig.vectorPeCol * arrayConfig.numMultiplier
    val originalDimensionSize = gemmDimension.n * gemmDimension.k

    val matrixRow = arrayConfig.dataflow match {
      case Dataflow.Is | Dataflow.Ws =>
        gemmDimension.k

      case Dataflow.Os =>
        if( originalDimensionSize % arrayColDimension  == 0)
          gemmDimension.k
        else
          originalDimensionSize  / arrayColDimension + 1

      case _ =>
        Console.err.println(s"[error] Invalid dataflow")
        sys.exit(1)
    }

    val matrixCol = arrayConfig.dataflow match {
      case Dataflow.Is | Dataflow.Ws =>
        gemmDimension.n

      case Dataflow.Os =>
        if( originalDimensionSize % arrayColDimension  == 0)
          gemmDimension.n
        else
          arrayColDimension

      case _ =>
        Console.err.println(s"[error] Invalid dataflow")
        sys.exit(1)
    }

    new TileB(
      layerName = layerName,
      id = getTileBId,
      MatrixDimension(DataType.B, arrayConfig.portBitWidth.typeB, matrixRow, matrixCol, loggerOption),
      loggerOption
    )
  }

  def generateTileC: TileC = {
    new TileC(
      layerName = layerName,
      id = getTileCId,
      MatrixDimension(DataType.C, arrayConfig.portBitWidth.typeC, gemmDimension.m, gemmDimension.n, loggerOption),
      loggerOption
    )
  }


  def printOperation() : Unit = {
    log(s"Layer Name: $layerName ID: $operationId")
    log(s"M: ${gemmDimension.m} N: ${gemmDimension.n} K: ${gemmDimension.k}")
    log(s"Tile A ID: $getTileAId")
    log(s"Tile B ID: $getTileBId")
    log(s"Tile C ID: $getTileCId")
    log("")
  }

}


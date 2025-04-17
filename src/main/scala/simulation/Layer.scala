package simulation

import common.Dataflow
import simulation.OffChipMemoryUploadOrder.OffChipMemoryUploadOrder

class Layer(
  val layerName: String,
  val gemmDimension: GemmDimension,
  val arrayConfig: ArrayConfig,
  val streamingDimensionSize: Int,
  val offChipMemoryUploadOrder: OffChipMemoryUploadOrder,
  val loggerOption: LoggerOption
) extends Logger {

  require(layerName.nonEmpty, "[error] Empty layer name is not allowed")
  require(streamingDimensionSize >= 1, "[error] Only positive integer is allowed for streaming dimension size")
  setMode(loggerOption)

  private val dimensionCoverageOfM: Int = initDimensionCoverageM
  private val dimensionCoverageOfN: Int = initDimensionCoverageN
  private val dimensionCoverageOfK: Int = initDimensionCoverageK

  private val dimensionCoverageQuotientOfM: Int = initDimensionCoverageQuotientM
  private val dimensionCoverageQuotientOfN: Int = initDimensionCoverageQuotientN
  private val dimensionCoverageQuotientOfK: Int = initDimensionCoverageQuotientK

  private val dimensionCoverageRemainderOfM: Int = initDimensionCoverageRemainderM
  private val dimensionCoverageRemainderOfN: Int = initDimensionCoverageRemainderN
  private val dimensionCoverageRemainderOfK: Int = initDimensionCoverageRemainderK

  private val tileIndexTuple: (Int, Int, Int) = calculateTileIndex
//  private val operationVector3D: Vector[Vector[Vector[MultiplicationOperation]]] = createOperation()

  val operationVector: Vector[MultiplicationOperation] = createOperation().flatten.flatten

  private def initDimensionCoverageM: Int = {
    arrayConfig.dataflow match {
      case Dataflow.Is => arrayConfig.groupPeRow * arrayConfig.vectorPeRow
      case Dataflow.Os => arrayConfig.groupPeRow * arrayConfig.vectorPeRow
      case Dataflow.Ws =>
        if(gemmDimension.m < streamingDimensionSize)
          gemmDimension.m
        else
          streamingDimensionSize
      case _ =>
        Console.err.println(s"[error] Invalid dataflow ${arrayConfig.dataflow}")
        sys.exit(1)
    }
  }

  private def initDimensionCoverageN: Int = {
    arrayConfig.dataflow match {
      case Dataflow.Is =>
        if(gemmDimension.n < streamingDimensionSize)
          gemmDimension.n
        else
          streamingDimensionSize
      case Dataflow.Os => arrayConfig.groupPeCol * arrayConfig.vectorPeCol
      case Dataflow.Ws => arrayConfig.groupPeCol * arrayConfig.vectorPeCol
      case _ =>
        Console.err.println(s"[error] Invalid dataflow ${arrayConfig.dataflow}")
        sys.exit(1)
    }
  }

  private def initDimensionCoverageK: Int = {
    arrayConfig.dataflow match {
      case Dataflow.Is => arrayConfig.groupPeCol * arrayConfig.vectorPeCol * arrayConfig.numMultiplier
      case Dataflow.Os =>
        if(gemmDimension.k < streamingDimensionSize)
          gemmDimension.k
        else
          streamingDimensionSize
      case Dataflow.Ws => arrayConfig.groupPeRow * arrayConfig.vectorPeRow * arrayConfig.numMultiplier
      case _ =>
        Console.err.println(s"[error] Invalid dataflow ${arrayConfig.dataflow}")
        sys.exit(1)
    }
  }

  private def initDimensionCoverageQuotientM: Int =
    gemmDimension.m / dimensionCoverageOfM

  private def initDimensionCoverageQuotientN: Int =
    gemmDimension.n / dimensionCoverageOfN

  private def initDimensionCoverageQuotientK: Int =
    gemmDimension.k / dimensionCoverageOfK

  private def initDimensionCoverageRemainderM: Int =
    gemmDimension.m % dimensionCoverageOfM

  private def initDimensionCoverageRemainderN: Int =
    gemmDimension.n % dimensionCoverageOfN

  private def initDimensionCoverageRemainderK: Int =
    gemmDimension.k % dimensionCoverageOfK

  private def calculateTileIndex: (Int, Int, Int) = {

    val M = if(dimensionCoverageRemainderOfM == 0 ) {
      dimensionCoverageQuotientOfM
    } else {
      dimensionCoverageQuotientOfM + 1
    }

    val N = if(dimensionCoverageRemainderOfN == 0) {
      dimensionCoverageQuotientOfN
    } else {
      dimensionCoverageQuotientOfN + 1
    }

    val K = if(dimensionCoverageRemainderOfK == 0){
      dimensionCoverageQuotientOfK
    } else {
      dimensionCoverageQuotientOfK + 1
    }

    (M, N, K)

  }

  //Create operation function determines the order of operations
  private def createOperation(): Vector[Vector[Vector[MultiplicationOperation]]] = {
    offChipMemoryUploadOrder match {
      case OffChipMemoryUploadOrder.mnk =>
        Vector.tabulate(tileIndexTuple._1, tileIndexTuple._2, tileIndexTuple._3)(
          (m,n,k) => new MultiplicationOperation(
            layerName,
            operationId = (m, n, k),
            createGemmDimension(m, n, k),
            arrayConfig,
//            portBitWidth,
            loggerOption
          )
        )
      case OffChipMemoryUploadOrder.mkn =>
        Vector.tabulate(tileIndexTuple._1, tileIndexTuple._3, tileIndexTuple._2)(
          (m,k,n) => new MultiplicationOperation(
            layerName,
            operationId = (m, n, k),
            createGemmDimension(m, n, k),
            arrayConfig,
//            portBitWidth,
            loggerOption
          )
        )
      case OffChipMemoryUploadOrder.nmk =>
        Vector.tabulate(tileIndexTuple._2, tileIndexTuple._1, tileIndexTuple._3)(
          (n,m,k) => new MultiplicationOperation(
            layerName,
            operationId = (m, n, k),
            createGemmDimension(m, n, k),
            arrayConfig,
//            portBitWidth,
            loggerOption
          )
        )
      case OffChipMemoryUploadOrder.nkm =>
        Vector.tabulate(tileIndexTuple._2, tileIndexTuple._3, tileIndexTuple._1)(
          (n,k,m) => new MultiplicationOperation(
            layerName,
            operationId = (m, n, k),
            createGemmDimension(m, n, k),
            arrayConfig,
//            portBitWidth,
            loggerOption
          )
        )
      case OffChipMemoryUploadOrder.kmn =>
        Vector.tabulate(tileIndexTuple._3, tileIndexTuple._1, tileIndexTuple._2)(
          (k,m,n) => new MultiplicationOperation(
            layerName,
            operationId = (m, n, k),
            createGemmDimension(m, n, k),
            arrayConfig,
//            portBitWidth,
            loggerOption
          )
        )
      case OffChipMemoryUploadOrder.knm =>
        Vector.tabulate(tileIndexTuple._3, tileIndexTuple._2, tileIndexTuple._1)(
          (k,n,m) => new MultiplicationOperation(
            layerName,
            operationId = (m, n, k),
            createGemmDimension(m, n, k),
            arrayConfig,
//            portBitWidth,
            loggerOption
          )
        )
      case _ =>
        Console.err.println("[error] sending wrong data tiles")
        sys.exit(1)
    }
  }

  private def createGemmDimension(indexM: Int, indexN: Int, indexK: Int): GemmDimension = {

    arrayConfig.dataflow match {
      case Dataflow.Is =>

        val sizeN = if(indexN < dimensionCoverageQuotientOfN)
          dimensionCoverageOfN
        else
          dimensionCoverageRemainderOfN

        GemmDimension(dimensionCoverageOfM, sizeN, dimensionCoverageOfK)

      case Dataflow.Os =>

        val sizeK = if(indexK < dimensionCoverageQuotientOfK)
          dimensionCoverageOfK
        else
          dimensionCoverageRemainderOfK

        GemmDimension(dimensionCoverageOfM, dimensionCoverageOfN, sizeK)

      case Dataflow.Ws =>
        val sizeM = if(indexM < dimensionCoverageQuotientOfM)
          dimensionCoverageOfM
        else
          dimensionCoverageRemainderOfM

        GemmDimension(sizeM, dimensionCoverageOfN, dimensionCoverageOfK)

      case _ =>
        Console.err.println("[error] sending wrong data tiles")
        sys.exit(1)
    }

  }

}

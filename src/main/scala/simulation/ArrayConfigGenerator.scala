package simulation

import common.{Dataflow, OutputPortCalculator}
import scala.util.{Failure, Success, Try}

object ArrayConfigGenerator extends OutputPortCalculator {

  case class DimensionConfig(r: Int, c: Int, a: Int, b: Int, p: Int) {
    def isValid(minDim: Int): Boolean =
      r >= minDim && c >= minDim &&
        isPowerOfTwo(p)

    def product: Int = r * c * a * b * p
  }

  private def isPowerOfTwo(n: Int): Boolean = {
    if( n <= 0 ) return false
    (n & (n-1)) == 0
  }

  private def getDivisors(n: Int): Vector[Int] =
    (1 to n).filter(n % _ == 0).toVector

  private def generateValidDimensionConfigs(
    multNumber: Int,
    minDimension: Int = 16
  ): Set[DimensionConfig] = {
    val divisors = getDivisors(multNumber)

    (for {
      r <- divisors
      c <- divisors
      a <- divisors
      b <- divisors
      p <- divisors
      config = DimensionConfig(r, c, a, b, p)
      if config.product == multNumber && config.isValid(minDimension)
    } yield config).toSet  // Using Set to automatically remove duplicates
  }

  //TODO do we need dnn model weight path in here?
  def generateArrayConfig(
    multNumber: Int,
    bitWidthPortA: Int,
    bitWidthPortB: Int,
    dataflow: Dataflow.Value,
    streamingDimensionSize: Int,
    dnnModelWeightsPath: Option[String] = None,
  ): Vector[ArrayConfig] = {

    val dimensionConfigs = generateValidDimensionConfigs(multNumber)

    val arrayConfigs = if(dnnModelWeightsPath.isDefined){

      val loadedModel = MAMLFewShotPredictor.loadModel(filePath = dnnModelWeightsPath.get)

      loadedModel match {
        case Success(modelWeights) =>

          dimensionConfigs.map { dim =>
            val outputPortBitWidth = calculateOutputPort(
              dataflow = dataflow,
              groupPeRow = dim.r,
              groupPeCol = dim.c,
              vectorPeRow = dim.a,
              vectorPeCol = dim.b,
              numMultiplier = dim.p,
              bitWidthPortA = bitWidthPortA,
              bitWidthPortB = bitWidthPortB,
              streamingDimensionSize = streamingDimensionSize
            )

            val synthesisData = MAMLFewShotPredictor.predictArraySynthesisData(
              dataflow = dataflow.toString,
              totalMultipliers = dim.product,
              groupPeRow = dim.r,
              groupPeCol = dim.c,
              vectorPeRow = dim.a,
              vectorPeCol = dim.b,
              numMultiplier = dim.p,
              streamingDimensionSize = streamingDimensionSize,
              model = modelWeights
            )

            ArrayConfig(
              groupPeRow = dim.r,
              groupPeCol = dim.c,
              vectorPeRow = dim.a,
              vectorPeCol = dim.b,
              numMultiplier = dim.p,
              dataflow = dataflow,
              portBitWidth = PortBitWidth(bitWidthPortA, bitWidthPortB, outputPortBitWidth),
              arraySynthesisData =  Some(synthesisData)
            )

          }

        case Failure(e) =>
          Console.err.println(s"Failed to load ML model: ${e.getMessage}")
          sys.exit(1)
      }

    } else {
      dimensionConfigs.map { dim =>
        val outputPortBitWidth = calculateOutputPort(
          dataflow = dataflow,
          groupPeRow = dim.r,
          groupPeCol = dim.c,
          vectorPeRow = dim.a,
          vectorPeCol = dim.b,
          numMultiplier = dim.p,
          bitWidthPortA = bitWidthPortA,
          bitWidthPortB = bitWidthPortB,
          streamingDimensionSize = streamingDimensionSize
        )

        ArrayConfig(
          groupPeRow = dim.r,
          groupPeCol = dim.c,
          vectorPeRow = dim.a,
          vectorPeCol = dim.b,
          numMultiplier = dim.p,
          dataflow = dataflow,
          portBitWidth = PortBitWidth(bitWidthPortA, bitWidthPortB, outputPortBitWidth),
        )
      }

    }

    arrayConfigs.toVector

  }

}



//
//    val arrayConfigs = dataflow match {
//      case Dataflow.Is =>
//        dimensionConfigs.map { dim =>
//          val bandWidthPortC = calculateOutputPort(
//            dataflow,
//            dim.r,
//            dim.c,
//            dim.a,
//            dim.b,
//            dim.p,
//            bandWidthPortA,
//            bandWidthPortB,
//            streamingDimensionSize
//          )
//          ArrayConfig(
//            dim.r,
//            dim.c,
//            dim.a,
//            dim.b,
//            dim.p,
//            dataflow,
//            PortBitWidth(bandWidthPortA, bandWidthPortB, bandWidthPortC)
//          )
//        }
//
//      case Dataflow.Os =>
//        dimensionConfigs.map { dim =>
//          val bandWidthPortC = calculateOutputPort(
//            dataflow,
//            dim.r,
//            dim.c,
//            dim.a,
//            dim.b,
//            dim.p,
//            bandWidthPortA,
//            bandWidthPortB,
//            streamingDimensionSize
//          )
//          ArrayConfig(
//            dim.r,
//            dim.c,
//            dim.a,
//            dim.b,
//            dim.p,
//            dataflow,
//            PortBitWidth(bandWidthPortA, bandWidthPortB, bandWidthPortC)
//          )
//        }
//
//      case Dataflow.Ws =>
//        dimensionConfigs.map { dim =>
//          val bandWidthPortC = calculateOutputPort(
//            dataflow,
//            dim.r,
//            dim.c,
//            dim.a,
//            dim.b,
//            dim.p,
//            bandWidthPortA,
//            bandWidthPortB,
//            streamingDimensionSize
//          )
//          ArrayConfig(
//            dim.r,
//            dim.c,
//            dim.a,
//            dim.b,
//            dim.p,
//            dataflow,
//            PortBitWidth(bandWidthPortA, bandWidthPortB, bandWidthPortC)
//          )
//        }
//
//      case _ =>
//        throw new IllegalArgumentException("Invalid dataflow")
//    }
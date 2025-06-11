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
    isRtlOnly: Boolean,
  ): Vector[ArrayConfig] = {

    val dimensionConfigs = generateValidDimensionConfigs(multNumber)

    val arrayConfigs = if(isRtlOnly){
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

        val capitalLetterDataflow = dataflow match {
          case Dataflow.Is => "IS"
          case Dataflow.Os => "OS"
          case Dataflow.Ws => "WS"
        }

        val totalMult = dim.r * dim.c * dim.a * dim.b * dim.p

        val synthesisData = FewShotPredictor.predict(
          FewShotPredictor.InputFeatures(
            dataflow = capitalLetterDataflow,
            totalNumberOfMultipliers = totalMult,
            r = dim.r,
            c = dim.c,
            a = dim.a,
            b = dim.b,
            p = dim.p,
            streamingDimensionSize = streamingDimensionSize
          )
        ) match {
          case Success(result) =>
//            println(s"✅ Prediction successful:")
//            println(f"   📏 Area: ${result.areaUm2}%,.1f µm²")
//            println(f"   🔋 Switch: ${result.switchPowerPw}%.2f mW")
//            println(f"   🔋 Internal: ${result.internalPowerPw}%.2f mW")
//            println(f"   🔋 Leakage: ${result.leakagePowerPw}%.2f mW")
            Some(result)
          case Failure(exception) =>
//            println(s"❌ Prediction failed: ${exception.getMessage}")
            throw new RuntimeException(s"Hardware prediction failed: ${exception.getMessage}")
        }

        ArrayConfig(
          groupPeRow = dim.r,
          groupPeCol = dim.c,
          vectorPeRow = dim.a,
          vectorPeCol = dim.b,
          numMultiplier = dim.p,
          dataflow = dataflow,
          portBitWidth = PortBitWidth(bitWidthPortA, bitWidthPortB, outputPortBitWidth),
          arraySynthesisData = synthesisData,
          arraySynthesisSource = Some(ArraySynthesisSource.FewShotPrediction),
        )
      }
    }

    arrayConfigs.toVector

  }

}

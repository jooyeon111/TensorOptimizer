package simulation

import common.{Dataflow, OutputPortCalculator}

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
    minDimension: Int = 8
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

  def generateArrayConfig(
    multNumber: Int,
    bandWidthPortA: Int,
    bandWidthPortB: Int,
    dataflow: Dataflow.Value,
    streamingDimensionSize: Int
  ): Vector[ArrayConfig] = {

    val dimensionConfigs = generateValidDimensionConfigs(multNumber)

    val arrayConfigs = dataflow match {
      case Dataflow.Is =>
        dimensionConfigs.map { dim =>
          val bandWidthPortC = calculateOutputPort(
            dataflow,
            dim.r,
            dim.c,
//            dim.a * dim.b,
//            1,
            dim.a,
            dim.b,
            dim.p,
            bandWidthPortA,
            bandWidthPortB,
            streamingDimensionSize
          )
          ArrayConfig(
            dim.r,
            dim.c,
//            dim.a * dim.b,
//            1,
            dim.a,
            dim.b,
            dim.p,
            dataflow,
            PortBitWidth(bandWidthPortA, bandWidthPortB, bandWidthPortC)
          )
        }

      case Dataflow.Os =>
        dimensionConfigs.map { dim =>
          val bandWidthPortC = calculateOutputPort(
            dataflow,
            dim.r,
            dim.c,
            dim.a,
            dim.b,
            dim.p,
            bandWidthPortA,
            bandWidthPortB,
            streamingDimensionSize
          )
          ArrayConfig(
            dim.r,
            dim.c,
            dim.a,
            dim.b,
            dim.p,
            dataflow,
            PortBitWidth(bandWidthPortA, bandWidthPortB, bandWidthPortC)
          )
        }

      case Dataflow.Ws =>
        dimensionConfigs.map { dim =>
          val bandWidthPortC = calculateOutputPort(
            dataflow,
            dim.r,
            dim.c,
//            1,
//            dim.a * dim.b,
            dim.a,
            dim.b,
            dim.p,
            bandWidthPortA,
            bandWidthPortB,
            streamingDimensionSize
          )
          ArrayConfig(
            dim.r,
            dim.c,
//            1,
//            dim.a * dim.b,
            dim.a,
            dim.b,
            dim.p,
            dataflow,
            PortBitWidth(bandWidthPortA, bandWidthPortB, bandWidthPortC)
          )
        }

      case _ =>
        throw new IllegalArgumentException("Invalid dataflow")
    }

    arrayConfigs.toVector
  }
}

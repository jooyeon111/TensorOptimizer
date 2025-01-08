package simulation

import scala.math.{ceil, log10}

//Remember modeling not only modeling the systolic tensor array itself, modeling pre- and post-processor too
case class ArrayConfig(
  groupPeRow: Int,
  groupPeCol: Int,
  vectorPeRow: Int,
  vectorPeCol: Int,
  numMultiplier: Int,
  dataflow: Dataflow.Value,
  portBitWidth: PortBitWidth,
) extends Logger {
  require(groupPeRow >= 1, "[error] Array row must be at least 1")
  require(groupPeCol >= 1, "[error] Array col must be at least 1")
  require(vectorPeRow >= 1, "[error] Block row must be at least 1")
  require(vectorPeCol >= 1, "[error] Block col must be at least 1")
  require(numMultiplier >= 1,  "[error] Number of multiplier inside of processing elements must be at least 1")
  require(dataflow == Dataflow.Is || dataflow == Dataflow.Os || dataflow == Dataflow.Ws,
    "[error] Currently only 3 dataflow are supported input, weight and output")

  val totalNumberOfMultipliers: Int = groupPeRow * groupPeCol * vectorPeRow * vectorPeCol * numMultiplier

  val dimensionOfInputA: Int = groupPeRow * vectorPeRow * numMultiplier
  val dimensionOfInputB: Int = groupPeCol * vectorPeCol * numMultiplier
  val dimensionOfOutput: Int = dataflow match {
    case Dataflow.Is => groupPeRow * vectorPeRow
    case Dataflow.Os => groupPeRow * vectorPeRow * vectorPeCol
    case Dataflow.Ws => groupPeCol * vectorPeCol
    case _ =>
      Console.err.println(s"[error] Invalid dataflow")
      sys.exit(1)
  }

  val bandwidthOfInputA: Int = dimensionOfInputA * portBitWidth.typeA
  val bandwidthOfInputB: Int = dimensionOfInputB * portBitWidth.typeB
  val outputBandwidth: Int = dimensionOfOutput * portBitWidth.typeC

  val capacityOfTileA: Int =
    dataflow match {
      case Dataflow.Is => groupPeRow * vectorPeRow * groupPeCol * vectorPeCol * numMultiplier * portBitWidth.typeA
      case Dataflow.Os => bandwidthOfInputA * ( 2 + ceil(log10(numMultiplier)/log10(2.0)).toInt) * portBitWidth.typeA
      case Dataflow.Ws => bandwidthOfInputA * ( 2 + ceil(log10(numMultiplier)/log10(2.0)).toInt) * portBitWidth.typeA
      case _ =>
        Console.err.println(s"[error] Invalid dataflow")
        sys.exit(1)
    }

  val capacityOfTileB: Int =
    dataflow match {
      case Dataflow.Is => bandwidthOfInputB * ( 2 + ceil(log10(numMultiplier)/log10(2.0)).toInt) * portBitWidth.typeB
      case Dataflow.Os => bandwidthOfInputB * ( 2 + ceil(log10(numMultiplier)/log10(2.0)).toInt) * portBitWidth.typeB
      case Dataflow.Ws => groupPeRow * vectorPeRow * groupPeCol * vectorPeCol * numMultiplier * portBitWidth.typeB
      case _ =>
        Console.err.println(s"[error] Invalid dataflow")
        sys.exit(1)
    }

  val name: String = s"${dataflow} {${groupPeRow}x$groupPeCol}x{${vectorPeRow}x$vectorPeCol}x$numMultiplier"

}

package simulation

import common.{Dataflow, ArrayDimension}
import scala.math.{ceil, log10}

//Remember modeling not only modeling the systolic tensor array itself, modeling pre- and post-processor too
case class ArrayConfig(
  override val groupPeRow: Int,
  override val groupPeCol: Int,
  override val vectorPeRow: Int,
  override val vectorPeCol: Int,
  override val numMultiplier: Int,
  dataflow: Dataflow.Value,
  portBitWidth: PortBitWidth,
) extends ArrayDimension(
  groupPeRow,
  groupPeCol,
  vectorPeRow,
  vectorPeCol,
  numMultiplier
) with Logger {

  require(dataflow == Dataflow.Is || dataflow == Dataflow.Os || dataflow == Dataflow.Ws,
    "[error] Currently only 3 dataflow are supported input, weight and output")

  val arrayConfigString: String = s"${dataflow.toString.toLowerCase}_$arrayDimensionString"
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



}

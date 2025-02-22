package common

import scala.math.{ceil, log}

trait OutputPortCalculator {

  private def multiplierOutputBitWidth(bitWidthPortA: Int, bitWidthPortB: Int): Int =
    bitWidthPortA + bitWidthPortB

  private def adderTreeOutputBitWidth(multiplierOutputBitWidth: Int, numMultiplier: Int): Int =
    multiplierOutputBitWidth + ceil(log(numMultiplier)/ log(2)).toInt

  protected def calculateOutputPort(
    dataflow: Dataflow.Value,
    groupPeRow: Int,
    groupPeCol: Int,
    vectorPeRow: Int,
    vectorPeCol: Int,
    numMultiplier: Int,
    bitWidthPortA: Int,
    bitWidthPortB: Int,
    streamingDimensionSize: Int = -1,
  ): Int = {

    val mBitWidth = multiplierOutputBitWidth(bitWidthPortA, bitWidthPortB)
    val adBitWidth = adderTreeOutputBitWidth(mBitWidth, numMultiplier)

    dataflow match {
      case Dataflow.Is =>
        adBitWidth + ceil(log( groupPeCol * vectorPeCol )/ log(2)).toInt

      case Dataflow.Os =>

        if(streamingDimensionSize != -1){
          val arrayRowDimension = groupPeRow * vectorPeRow * numMultiplier
          val mappedDimensionSize = groupPeRow * vectorPeRow * streamingDimensionSize

          val additionalAdditionNumber = if(mappedDimensionSize % arrayRowDimension == 0){
            streamingDimensionSize
          } else {
            mappedDimensionSize / arrayRowDimension + 1
          }

          adBitWidth + ceil(log(additionalAdditionNumber)/log(2)).toInt
        } else {
          Console.err.println("Output stationary must need output port bandwidth")
          sys.exit(1)
        }


      case Dataflow.Ws =>
        adBitWidth + ceil(log( groupPeRow * vectorPeRow )/ log(2)).toInt
    }
  }

  protected def calculatePortBitWidthInfo(
    dataflow: Dataflow.Value,
    groupPeRow: Int,
    groupPeCol: Int,
    vectorPeRow: Int,
    vectorPeCol: Int,
    numMultiplier: Int,
    bitWidthPortA: Int,
    bitWidthPortB: Int,
    configPortC: Option[Int],
    streamingDimensionSize: Int = -1,
  ): PortBitWidthInfo = {
    var enableUserBitWidth = true
    val bitWidthMultiplierOutput = multiplierOutputBitWidth(bitWidthPortA, bitWidthPortB)
    val bitWidthAdderTreeOutput = adderTreeOutputBitWidth(bitWidthMultiplierOutput, numMultiplier)

    val bitWidthPortC = configPortC.getOrElse{
      enableUserBitWidth = false
      calculateOutputPort(
        dataflow,
        groupPeRow,
        groupPeCol,
        vectorPeRow,
        vectorPeCol,
        numMultiplier,
        bitWidthPortA,
        bitWidthPortB,
        streamingDimensionSize,
      )
    }
//    println()
//    println(s"Group PE row * PE Col: ($groupPeRow * $groupPeCol)")
//    println(s"Vector PE row * PE Col: ($vectorPeRow * $vectorPeCol)")
//    println(s"Multipliers per PE: ${numMultiplier}")
//    println(s"SD size: $streamingDimensionSize")
//    println(s"Multiplier output bitwidth: $bitWidthMultiplierOutput")
//    println(s"Adder tree output bitwidth: $bitWidthAdderTreeOutput")
//    println(s"PE output bit width: $bitWidthPortC")

    PortBitWidthInfo(
      bitWidthPortA,
      bitWidthPortB,
      bitWidthMultiplierOutput,
      bitWidthAdderTreeOutput,
      enableUserBitWidth,
      bitWidthPortC
    )

  }

}

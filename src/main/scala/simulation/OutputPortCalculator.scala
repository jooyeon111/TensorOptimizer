package simulation

import scala.math.{ceil, log}

trait OutputPortCalculator {

  protected def calculateOutputPort(
    dataflow: Dataflow.Value,
    groupPeRow: Int,
    groupPeCol: Int,
    vectorPeRow: Int,
    vectorPeCol: Int,
    numMultiplier: Int,
    bitWidthPortA: Int,
    bitWidthPortB: Int,
    streamingDimensionSize: Int,
  ): Int = {
    val multiplierOutputBitWidth = bitWidthPortA + bitWidthPortB
    val adderTreeOutputBitWidth = multiplierOutputBitWidth + ceil(log(numMultiplier) / log(2)).toInt
    dataflow match {
      case Dataflow.Is =>
        adderTreeOutputBitWidth + ceil(log( groupPeCol * vectorPeCol )/ log(2)).toInt

      case Dataflow.Os =>
        val arrayRowDimension = groupPeRow * vectorPeRow * numMultiplier
        val originalDimensionSize = groupPeRow * vectorPeRow * streamingDimensionSize

        val additionalAdditionNumber = if(originalDimensionSize % arrayRowDimension == 0){
          streamingDimensionSize
        } else {
          originalDimensionSize / arrayRowDimension + 1
        }

        adderTreeOutputBitWidth + additionalAdditionNumber

      case Dataflow.Ws =>
        adderTreeOutputBitWidth + ceil(log( groupPeRow * vectorPeRow )/ log(2)).toInt

    }


  }


}

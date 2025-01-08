package simulation

trait StreamingDimensionCalculator {
  def getMaximumStreamingDimension(gemmDimension : GemmDimension, dataflow: Dataflow.Value) : Int = {

    dataflow match {
      case Dataflow.Is =>
        gemmDimension.n
      case Dataflow.Os =>
        gemmDimension.k
      case Dataflow.Ws =>
        gemmDimension.m
    }

  }
}
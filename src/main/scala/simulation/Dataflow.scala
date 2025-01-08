package simulation

object Dataflow extends Enumeration {
  type Dataflow = Value
  val Is: Dataflow = Value("Input Stationary")
  val Os: Dataflow = Value("Output Stationary")
  val Ws: Dataflow = Value("Weight Stationary")
}

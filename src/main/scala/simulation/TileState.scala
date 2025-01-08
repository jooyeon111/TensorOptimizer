package simulation

object TileState extends Enumeration{
  type TileState = Value
  val waiting, nextCalculation, loading, loaded, calculating, calculated = Value
}

package simulation

object TileDeduplicationStrategy extends Enumeration {
  type TileDeduplicationStrategy = Value
  val Blind, OneEye, TwoEye = Value

}

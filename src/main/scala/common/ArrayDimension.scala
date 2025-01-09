package common

class ArrayDimension(
  val groupPeRow: Int,
  val groupPeCol: Int,
  val vectorPeRow: Int,
  val vectorPeCol: Int,
  val numMultiplier: Int
) {

  val arrayDimensionString = s"${groupPeRow}x${groupPeCol}x${vectorPeRow}x${vectorPeCol}x$numMultiplier"

  require(groupPeRow >= 1, "Array row must be at least 1")
  require(groupPeCol >= 1, "Array col must be at least 1")
  require(vectorPeRow >= 1, "Block row must be at least 1")
  require(vectorPeCol >= 1, "Block col must be at least 1")
  require(numMultiplier >= 1, "Total number of multipliers must be at least 1")

}

package simulation

import simulation.DataType.DataType

case class MatrixDimension(
  dataType: DataType,
  bandwidth: Int,
  row: Int,
  col: Int,
  loggerOption: LoggerOption
) extends Logger{

  require(bandwidth >= 1 , "[error] Bandwidth must be at least 1")
  require(row >= 1 , "[error] Row must be at least 1")
  require(col >= 1 , "[error] Col must be at least 1")
  setMode(loggerOption)

  val memorySize: Int = bandwidth * row * col

  def printDimension(): Unit = {
    log(s"\t\trow: $row col: $col Bit-width: $bandwidth memory size: $memorySize")
  }

}
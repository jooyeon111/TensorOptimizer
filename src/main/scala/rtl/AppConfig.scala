package rtl

import common.{Dataflow, ArrayDimension}

case class AppConfig (
  splitVerilogOutput: Boolean,
  dataflow: Dataflow.Value,
  arrayDimension: ArrayDimension,
  integerType: IntegerType.Value,
  portBitWidthInfo: PortBitWidthInfo
)
package common

case class VerilogGenerationConfig (
  splitVerilogOutput: Boolean,
  dataflow: Dataflow.Value,
  arrayDimension: ArrayDimension,
  integerType: IntegerType.Value,
  portBitWidthInfo: PortBitWidthInfo
)
package rtl

import common.Dataflow
import rtl.commonRtl.SystolicTensorArrayConfig

case class AppConfig (
  splitVerilogOutput: Boolean,
  dataflow: Dataflow.Value,
  arrayConfig: SystolicTensorArrayConfig,
  integerType: IntegerType.Value,
  portBitWidthInfo: PortBitWidthInfo
)
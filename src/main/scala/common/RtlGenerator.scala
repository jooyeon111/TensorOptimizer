package common

import chisel3._
import circt.stage.ChiselStage
import rtl.commonRtl.{Arithmetic, PortConfig}

trait RtlGenerator {

  def generateRtl(verilogGenerationConfig: VerilogGenerationConfig): Unit = {
    val VerilogGenerationConfig(splitVerilogOutput, dataflow, arrayConfig, integerType, portBitWidthInfo, streamingDimensionSize) = verilogGenerationConfig

    integerType match {
      case IntegerType.Signed =>
        generateRtlForType[SInt](splitVerilogOutput, arrayConfig, portBitWidthInfo, dataflow, streamingDimensionSize, (w: Int) => SInt(w.W))

      case IntegerType.UnSigned =>
        generateRtlForType[UInt](splitVerilogOutput, arrayConfig, portBitWidthInfo, dataflow, streamingDimensionSize, (w: Int) => UInt(w.W))
    }
  }

  private def generateRtlForType[T <: Data](
    splitVerilogOutput: Boolean,
    arrayConfig: ArrayDimension,
    portBitWidthInfo: PortBitWidthInfo,
    dataflow: Dataflow.Value,
    streamingDimensionSize: Int,
    typeConstructor: Int => T
  )(implicit ev: Arithmetic[T]): Unit = {

    val inputTypeA = typeConstructor(portBitWidthInfo.bitWidthPortA)
    val inputTypeB = typeConstructor(portBitWidthInfo.bitWidthPortB)
    val multiplierOutputType = typeConstructor(portBitWidthInfo.bitWidthMultiplierOutput)
    val adderTreeOutputType = typeConstructor(portBitWidthInfo.bitWidthAdderTreeOutput)
    val outputTypeC = typeConstructor(portBitWidthInfo.bitWidthPortC)

    val portConfig = new PortConfig(
      inputTypeA,
      inputTypeB,
      multiplierOutputType,
      adderTreeOutputType,
      portBitWidthInfo.enableUserBitWidth,
      outputTypeC
    )

    val dataflowString = dataflow.toString.toLowerCase
//    val generatedFileName = s"${dataflowString}_sta_${arrayConfig.arrayDimensionString}"

    val generatedFileName = dataflow match {
      case Dataflow.Is | Dataflow.Ws=>
        s"${dataflowString}_sta_${arrayConfig.arrayDimensionString}"
      case Dataflow.Os =>
        s"${dataflowString}_sta_${arrayConfig.arrayDimensionString}_sd$streamingDimensionSize"
    }


    lazy val rtlGenerator =  dataflow match {
      case Dataflow.Is =>
        new rtl.input.DimensionAlignedSystolicTensorArray(arrayConfig, generatedFileName, portConfig)
      case Dataflow.Os =>
        new rtl.output.DimensionAlignedSystolicTensorArray(arrayConfig, generatedFileName, portConfig)
      case Dataflow.Ws =>
        new rtl.weight.DimensionAlignedSystolicTensorArray(arrayConfig, generatedFileName, portConfig)
    }

    if(splitVerilogOutput){
      ChiselStage.emitSystemVerilogFile(
        rtlGenerator,
        firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", s"-o=${FilePaths.resourcesOutputRtl}/$generatedFileName/", "-split-verilog")
      )
    } else {
      ChiselStage.emitSystemVerilogFile(
        rtlGenerator,
        firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", s"-o=${FilePaths.resourcesOutputRtl}/$generatedFileName.sv")
      )
    }
  }

}

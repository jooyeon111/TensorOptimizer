package rtl.input

import chisel3._
import common.ArrayDimension
import rtl.commonRtl.{Arithmetic, PortConfig, PreProcessor, PreProcessorType}

class DimensionAlignedSystolicTensorArray[ T <: Data ] (
  groupPeRow: Int,
  groupPeCol : Int,
  vectorPeRow : Int,
  vectorPeCol : Int,
  numMultiplier : Int,
  dedicatedName: String,
  portConfig: PortConfig[T],
)(implicit ev: Arithmetic[T]) extends Module {

  def this(arrayDimension: ArrayDimension, dedicatedName: String, portConfig: PortConfig[T])(implicit ev: Arithmetic[T]) =
    this(
      arrayDimension.groupPeRow,
      arrayDimension.groupPeCol,
      arrayDimension.vectorPeRow,
      arrayDimension.vectorPeCol,
      arrayDimension.numMultiplier,
      dedicatedName,
      portConfig
    )

  override def desiredName: String = dedicatedName

  val numInputA: Int = groupPeRow * vectorPeRow * numMultiplier
  val numInputB: Int = groupPeCol * vectorPeCol * numMultiplier
  val numPropagateA: Int = groupPeCol * vectorPeCol
  val numOutput : Int = groupPeRow * vectorPeRow

  val preProcessorInputA = Module (new PreProcessor(
    groupPeRow,
    vectorPeRow,
    numMultiplier,
    skewFlag = false,
    PreProcessorType.A,
    portConfig.inputTypeA
  ))
  val preProcessorInputB = Module (new PreProcessor(
    groupPeCol,
    vectorPeCol,
    numMultiplier,
    skewFlag = true,
    PreProcessorType.B,
    portConfig.inputTypeB
  ))
  val systolicTensorArray = Module (new SystolicTensorArray(
    groupPeRow,
    groupPeCol,
    vectorPeRow,
    vectorPeCol,
    numMultiplier,
    portConfig,
  ))
  val postProcessor = Module ( new PostProcessor(
    groupPeRow,
    vectorPeRow,
    systolicTensorArray.outputTypeC
  ))

  val io = IO(new Bundle {
    val inputA = Input(Vec(numInputA, portConfig.inputTypeA))
    val inputB = Input(Vec(numInputB, portConfig.inputTypeB))
    val propagateA = Input(Vec(numPropagateA, Bool()))
    val outputC = Output(Vec(numOutput, systolicTensorArray.outputTypeC))
  })

  //Wiring Input
  preProcessorInputA.io.input := io.inputA
  preProcessorInputB.io.input := io.inputB
  systolicTensorArray.io.inputA := preProcessorInputA.io.output
  systolicTensorArray.io.inputB := preProcessorInputB.io.output
  postProcessor.io.input := systolicTensorArray.io.outputC

  //Wiring Control
  systolicTensorArray.io.propagateA := RegNext(io.propagateA, VecInit.fill(numPropagateA)(false.B))

  //Wiring Output
  io.outputC := postProcessor.io.output



}

package rtl.output

import chisel3._
import common.ArrayDimension
import rtl.commonRtl.{Arithmetic, PortConfig, PreProcessor, PreProcessorType, VerilogNaming}

//Pod = Pre Processing Unit +  Systolic Tensor Array + Post Processing Unit
class SkewedSystolicTensorArray[T <: Data](
  groupPeRow: Int,
  groupPeCol : Int,
  vectorPeRow : Int,
  vectorPeCol : Int,
  numMultiplier : Int,
  dedicatedName: String,
  portConfig: PortConfig[T],
)(implicit ev: Arithmetic[T]) extends Module with VerilogNaming
 {
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
  val numPartialSumReset = groupPeRow * groupPeCol
  val numPropagateOutput = (groupPeRow - 1) * (groupPeCol - 1)

  val numOutput: Int = (groupPeCol + groupPeRow - 1)* vectorPeRow * vectorPeCol
  val outputTypeC = portConfig.getStaOutputTypeC

  val preProcessorInputA = Module (new PreProcessor(
    groupPeRow,
    vectorPeRow,
    numMultiplier,
    skewFlag = true,
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
    portConfig))

  val io = IO(new Bundle {
    val inputA = Input(Vec(numInputA, portConfig.inputTypeA))
    val inputB = Input(Vec(numInputB, portConfig.inputTypeB))
    val propagateOutput =  Input(Vec(numPropagateOutput, Bool()))
    val partialSumReset =  Input(Vec(numPartialSumReset, Bool()))
    val outputC = Output(Vec(numOutput, outputTypeC))
  })

  //Wiring Input
  preProcessorInputA.io.input := io.inputA
  preProcessorInputB.io.input := io.inputB
  systolicTensorArray.io.inputA := preProcessorInputA.io.output
  systolicTensorArray.io.inputB := preProcessorInputB.io.output

  //Wiring propagate signal
  systolicTensorArray.io.partialSumReset := RegNext(io.partialSumReset, VecInit.fill(numPartialSumReset)(false.B))

  //Wiring partial sum signals
  systolicTensorArray.io.propagateOutput := RegNext( io.propagateOutput, VecInit.fill(numPropagateOutput)(false.B))


  //Wiring Output
   io.outputC := systolicTensorArray.io.outputC

}

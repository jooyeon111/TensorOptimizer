package rtl.commonRtl

import chisel3._

class Mac[T <: Data](
  numMultiplier: Int,
  inputTypeA: T,
  inputTypeB: T,
  multiplierOutputType: T,
  adderTreeOutputType: T,
)( implicit ev: Arithmetic[T]) extends Module with VerilogNaming {

  override val desiredName:String = camelToSnake(this.getClass.getSimpleName)

  require(numMultiplier >= 2, " At least 2 number of multipliers are needed for multiply and accumulation logic")

  val io = IO (new Bundle {
    val inputA = Input(Vec(numMultiplier, inputTypeA))
    val inputB = Input(Vec(numMultiplier, inputTypeB))
    val output = Output(adderTreeOutputType)
  })

  val multiplier = Module(new ParallelMultiplier(numMultiplier, inputTypeA, inputTypeB, multiplierOutputType))
  val adderTree = Module(new AdderTree(numMultiplier, multiplierOutputType, adderTreeOutputType))

  multiplier.io.inputA := io.inputA
  multiplier.io.inputB := io.inputB
  adderTree.io.input := multiplier.io.output
  io.output := adderTree.io.output

}

package rtl.commonRtl

import chisel3._

class ParallelMultiplier[T <: Data](
  numMultiplier: Int,
  inputTypeA: T,
  inputTypeB: T,
  outputType: T
)(implicit ev: Arithmetic[T]) extends Module with VerilogNaming {

  override def desiredName: String = camelToSnake( if(numMultiplier == 1) "Multiplier" else "ParallelMultiplier" )

  val io = IO(new Bundle {
    val inputA = Input(Vec(numMultiplier, inputTypeA ))
    val inputB = Input(Vec(numMultiplier, inputTypeB ))
    val output = Output(Vec(numMultiplier, outputType ))
  })

  for(i <- 0 until numMultiplier)
    io.output(i) := RegNext(ev.multiply(io.inputA(i), io.inputB(i)), ev.zero(outputType.getWidth))

}



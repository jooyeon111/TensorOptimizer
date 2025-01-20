package simulation

class Interface(
  val dram: Dram,
  val sramA: DoubleBufferSram,
  val sramB: DoubleBufferSram,
  val sramC: OutputDoubleBufferSram,
  val array: Array,
) {

  require(sramA.dataType == DataType.A, "Double buffering SRAM A data type dose not match")
  require(sramB.dataType == DataType.B, "Double buffering SRAM B data type dose not match")
  require(sramC.dataType == DataType.C, "Double buffering SRAM B data type dose not match")

  private var cycle: Long = 0

  def updateCycle(currentCycle: Long): Unit = {
    cycle = currentCycle
  }

  def getCycle: Long = cycle

  def checkTraffic(): Either[RunTimeError, Unit] = {

    val isTransmissionStuck = dram.isStuck &&
      sramA.isStuck &&
      sramB.isStuck &&
      array.isStuck &&
      sramC.isStuck &&
      sramA.isFirstFillUpCompete &&
      sramB.isFirstFillUpCompete

    if(isTransmissionStuck) {
      Left(RunTimeError("Hardware is stuck !!!"))
    } else {
      Right(())
    }
  }

}

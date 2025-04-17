package simulation

class Interface(
  val offChipMemory: OffChipMemory,
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

    val isTransmissionStuck = offChipMemory.isFailedToSend &&
      sramA.isFailedToSend &&
      sramB.isFailedToSend &&
      array.isFailedToSend &&
      sramC.isFailedToSend &&
      sramA.isFirstFillUpDone &&
      sramB.isFirstFillUpDone &&
      !array.canArrayReceiveTileFromSramAfterBufferChangeA &&
      !array.canArrayReceiveTileFromSramAfterBufferChangeB

    if(isTransmissionStuck) {
      Left(RunTimeError("All Hardware can not send tile ...! Check out the log file please~"))
    } else {
      Right(())
    }
  }

}

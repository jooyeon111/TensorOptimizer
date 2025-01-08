package simulation

case class DramReferenceData(
  readEnergyPj: Double, //pJ
  writeEnergyPj: Double, //pJ
  leakagePowerMw: Double, //mW
) {
  def validate: Boolean = {
      readEnergyPj > 0 &&
      writeEnergyPj > 0 &&
      leakagePowerMw > 0
  }
  val leakagePowerPw: Double = leakagePowerMw * 1e9
  //TODO do i make it as a parameter?
  val areaMm2: Double = 75.0
}

package simulation

case class DramReferenceData(
  readEnergyPj: Double, //pJ
  writeEnergyPj: Double, //pJ
) {
  def validate: Boolean = {
      readEnergyPj > 0 &&
      writeEnergyPj > 0
  }

  val areaMm2: Double = 75.0
}

package simulation

case class SramReferenceData(
  capacityKb: Int, // KB
  bandwidthBytes: Int, // bytes
  readEnergyPj: Double, // pJ
  writeEnergyPj: Double, // pJ
  leakagePowerMw: Double, // mW
  areaUm2: Double// um^2
) {
  def validate: Boolean = {
    capacityKb > 0 &&
      bandwidthBytes > 0 &&
      readEnergyPj > 0 &&
      writeEnergyPj > 0 &&
      leakagePowerMw > 0
  }
  val leakagePowerPw: Double = leakagePowerMw * 1e9
  val bandwidthBits: Int = bandwidthBytes * 8
}

package simulation

case class ArraySynthesisData(
  areaUm2: Double,
  private val switchPowerMw: Double,
  private val internalPowerMw: Double,
  private val leakagePowerMw: Double,
) {

  val switchPowerPw: Double = switchPowerMw * 1e9
  val internalPowerPw: Double = internalPowerMw * 1e9
  val leakagePowerPw: Double = leakagePowerMw * 1e9
  val totalPower: Double = switchPowerPw + internalPowerPw + leakagePowerPw

  def validate: Boolean = {
    areaUm2 > 0.0 &&
      switchPowerMw > 0.0 &&
      internalPowerMw > 0.0 &&
      leakagePowerMw > 0.0
  }

}

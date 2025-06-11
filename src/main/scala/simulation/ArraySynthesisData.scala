package simulation

object ArraySynthesisData {

  def apply(
    areaUm2: Double,
    totalPowerMw: Double,
  ): ArraySynthesisData = {

    require(areaUm2 > 0.0, "Area must be positive")
    require(totalPowerMw > 0.0, "Total power must be positive")

    val switchPowerMw = totalPowerMw * 0.015
    val internalPowerMw = totalPowerMw * 0.97
    val leakagePowerMw = totalPowerMw * 0.015

    new ArraySynthesisData(areaUm2, switchPowerMw, internalPowerMw, leakagePowerMw)
  }

}

case class ArraySynthesisData(
  areaUm2: Double,
  private val switchPowerMw: Double,
  private val internalPowerMw: Double,
  private val leakagePowerMw: Double,
) {

  val switchPowerPw: Double = switchPowerMw * 1e9
  val internalPowerPw: Double = internalPowerMw * 1e9
  val leakagePowerPw: Double = leakagePowerMw * 1e9
  val totalPowerPw: Double = switchPowerPw + internalPowerPw + leakagePowerPw
  val totalPowerMw: Double = switchPowerMw + internalPowerMw + leakagePowerMw

  def validate: Boolean = {
    areaUm2 > 0.0 &&
      switchPowerMw > 0.0 &&
      internalPowerMw > 0.0 &&
      leakagePowerMw > 0.0
  }

}

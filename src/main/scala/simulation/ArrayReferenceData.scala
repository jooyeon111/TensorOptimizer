package simulation

case class ArrayReferenceData(
  //mW
  leakagePowerGroupPeRowMw: Double,
  leakagePowerGroupPeColMw: Double,
  leakagePowerVectorPeRowMw: Double,
  leakagePowerVectorPeColMw: Double,
  leakagePowerNumMultiplierMw: Double,
  //pJ
  dynamicPowerGroupPeRowPj: Double,
  dynamicPowerGroupPeColPj: Double,
  dynamicPowerVectorPeRowPj: Double,
  dynamicPowerVectorPeColPj: Double,
  dynamicPowerNumMultiplierPj: Double,
  //um^2
  areaPowerGroupPeRowUm2: Double,
  areaPowerGroupPeColUm2: Double,
  areaPowerVectorPeRowUm2: Double,
  areaPowerVectorPeColUm2: Double,
  areaPowerNumMultiplierUm2: Double,
) {

  def validate: Boolean = {
    //mW
    leakagePowerGroupPeRowMw > 0 &&
    leakagePowerGroupPeColMw > 0 &&
    leakagePowerVectorPeRowMw > 0 &&
    leakagePowerVectorPeColMw > 0 &&
    leakagePowerNumMultiplierMw > 0 &&
    //pJ
    dynamicPowerGroupPeRowPj > 0 &&
    dynamicPowerGroupPeColPj > 0 &&
    dynamicPowerVectorPeRowPj > 0 &&
    dynamicPowerVectorPeColPj > 0 &&
    dynamicPowerNumMultiplierPj > 0 &&
      //um^2
    areaPowerGroupPeRowUm2 > 0 &&
    areaPowerGroupPeColUm2 > 0 &&
    areaPowerVectorPeRowUm2 > 0 &&
    areaPowerVectorPeColUm2 > 0 &&
    areaPowerNumMultiplierUm2 > 0
  }

  val leakagePowerGroupPeRowPw: Double = leakagePowerGroupPeRowMw * 1e9
  val leakagePowerGroupPeColPw: Double = leakagePowerGroupPeColMw * 1e9
  val leakagePowerVectorPeRowPw: Double = leakagePowerVectorPeRowMw * 1e9
  val leakagePowerVectorPeColPw: Double = leakagePowerVectorPeColMw * 1e9
  val leakagePowerNumMultiplierPw: Double = leakagePowerNumMultiplierMw * 1e9

}

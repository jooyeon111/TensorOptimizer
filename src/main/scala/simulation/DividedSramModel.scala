package simulation

case class DividedSramModel(
  bankCount: Int,
  referenceData: SramReferenceData,
) {
  val totalSramCapacityKb: Int = bankCount * referenceData.capacityKb
  val totalSramSizeUm2: Double = bankCount * referenceData.areaUm2
}

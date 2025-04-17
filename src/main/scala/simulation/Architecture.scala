package simulation

import common.Dataflow

case class Architecture(
  arrayConfig: ArrayConfig,
  streamingDimensionSize: Int,
  singleBufferLimitKbA: Int,
  singleBufferLimitKbB: Int,
  singleBufferLimitKbC: Int,
) {

  val offChipMemoryUploadOrder: OffChipMemoryUploadOrder.Value = arrayConfig.dataflow match {
    case Dataflow.Is => OffChipMemoryUploadOrder.mkn
    case Dataflow.Os => OffChipMemoryUploadOrder.mnk
    case Dataflow.Ws => OffChipMemoryUploadOrder.knm
  }

  def withStreamingDimensionSize(newSize: Int): Architecture =
    copy(streamingDimensionSize = newSize)

  def withUniformSramSizes(newSize: Int): Architecture = {
    copy(singleBufferLimitKbA = newSize, singleBufferLimitKbB = newSize, singleBufferLimitKbC = newSize)
  }

  def withSramBufferSize(newSize: Int, sramType: DataType.Value): Architecture = {
    sramType match {
      case DataType.A => copy(singleBufferLimitKbA = newSize)
      case DataType.B => copy(singleBufferLimitKbB = newSize)
      case DataType.C => copy(singleBufferLimitKbC = newSize)
    }
  }

  def isAllSramSameSize: Boolean = {
    (singleBufferLimitKbA == singleBufferLimitKbB) && (singleBufferLimitKbB == singleBufferLimitKbC)
  }

}
package simulation

private case class TileScheduleEntry(
    id: (Int,Int),
    var isScheduled: Boolean = false,
  ) {
  def markAsScheduled(): Unit = {
    isScheduled = true
  }
}
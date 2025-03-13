package simulation

private case class TileScheduleEntry(
    id: (Int,Int),
    var isInReadBuffer: Boolean = false,
//    var isInWriteBuffer: Boolean = false,
  ) {
  def markAsInReadBuffer(): Unit = {
    require(!isInReadBuffer, "Tile Operation Double Check!!!")
    isInReadBuffer = true
  }

//  def markAsInWriteBuffer(): Unit = {
//    require(!isInWriteBuffer, "Tile Operation Double Check!!!")
//    isInWriteBuffer = true
//  }

}
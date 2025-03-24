package simulation

import scala.collection.mutable

//TODO Delete it
trait PatternMatcher {

  def matchPatterns(
    writeBufferPattern: mutable.Queue[(Int, Int)],
    tileOperationOrder: mutable.ListBuffer[TileScheduleEntry]
  ): Unit = {
    while(writeBufferPattern.nonEmpty){

      val startIndex = tileOperationOrder.indexWhere(!_.isInReadBuffer)
      if(startIndex < 0) return

      if(writeBufferPattern.head != tileOperationOrder(startIndex).id){
        return
      }

      var index = startIndex
      var patternIndex = 0

      while(patternIndex < writeBufferPattern.size) {
        val tileId = writeBufferPattern(patternIndex)
        while(index < tileOperationOrder.length && tileId == tileOperationOrder(index).id) {
          tileOperationOrder(index).markAsInReadBuffer()
          index += 1
        }
        patternIndex += 1
      }

      val schedulePattern = mutable.Queue[(Int, Int)]()
      val patternRange = startIndex until index
      schedulePattern ++= patternRange.map(i => tileOperationOrder(i).id)
      val patternSize = schedulePattern.size

      @scala.annotation.tailrec
      def findAndMarkPattern(currentIndex: Int): Unit = {
//        println("Find and mark pattern")
        if(currentIndex + patternSize <= tileOperationOrder.length) {
          val isMatched = (0 until patternSize).forall(i =>
            tileOperationOrder(currentIndex + i).id == tileOperationOrder(startIndex + i).id
          )
          if(isMatched) {
            (0 until patternSize).foreach(i =>
              tileOperationOrder(currentIndex + i).markAsInReadBuffer()
            )
            findAndMarkPattern(currentIndex + patternSize)
          }
        }
      }

      findAndMarkPattern(index)

      (0 until schedulePattern.toSet.size).foreach(_ => writeBufferPattern.dequeue())


    }
  }

}

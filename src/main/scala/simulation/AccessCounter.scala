package simulation

trait AccessCounter {

  private var readAccessCount: Long = 0
  private var writeAccessCount: Long = 0

  def incrementReadAccessCount(): Unit = readAccessCount += 1
  def incrementWriteAccessCount(): Unit = writeAccessCount += 1

  def getReadAccessCount: Long = readAccessCount
  def getWriteAccessCount: Long = writeAccessCount

}

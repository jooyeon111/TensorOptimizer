package simulation

trait Hardware {
  //TODO modify stuck logic
  protected var stuck: Boolean = false
//  protected var readAccessCount = 0
//  protected var writeAccessCount = 0
  protected def update(interface: Interface) : Unit
  protected def updateState(): Unit
  protected def send(interface: Interface): Unit
  protected def prepareTileForSend(): Unit
  def restoreTrafficState(): Unit = {
    stuck = false
  }
  def isStuck: Boolean = stuck
  def printTiles() : Unit
  def isHardwareEmpty: Boolean
//  final def getReadAccessCount: Int = readAccessCount
//  final def getWriteAccessCount: Int = writeAccessCount
//
//  final def resetAccessCount(): Unit = {
//    readAccessCount = 0
//    writeAccessCount = 0
//  }

}

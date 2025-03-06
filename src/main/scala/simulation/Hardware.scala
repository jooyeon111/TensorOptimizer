package simulation

trait Hardware {

  protected def update(interface: Interface) : Unit
  protected def updateState(): Unit
  protected def send(interface: Interface): Unit
  protected def prepareTileForSend(): Unit

  private var failedToSend: Boolean = false
  def isFailedToSend: Boolean = failedToSend
  protected def markTileSendFailed(): Unit = failedToSend = true
  protected def markTileSendSuccessful(): Unit = failedToSend = false

  private var readyToSend: Boolean = true
  def isReadyToSend: Boolean = readyToSend
  def pauseTileSending(): Unit = readyToSend = false
  def resumeTileSending(): Unit = readyToSend = true

  def printTiles() : Unit
  def isHardwareEmpty: Boolean

}

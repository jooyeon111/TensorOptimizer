package simulation

sealed trait Error extends Throwable {
  def message: String
  override def getMessage: String = message
}
case class RunTimeError(message: String) extends Error
case class ParseError(message: String) extends Error
case class CompBuildError(message: String) extends Error

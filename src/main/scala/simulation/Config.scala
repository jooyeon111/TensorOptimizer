package simulation

case class Config(values: Map[String, String]) {

  def getString(key: String): Option[String] = values.get(key)
  def getInt(key: String): Option[Int] = values.get(key).map(_.toInt)
  def getBoolean(key: String): Option[Boolean] = values.get(key).map(_.toBoolean)

}

package simulation

case class PortBitWidth(
  typeA: Int,
  typeB: Int,
  typeC: Int,
) {

  def validate: Boolean = {
    typeA > 0 && typeB > 0 && typeC > 0
  }

}

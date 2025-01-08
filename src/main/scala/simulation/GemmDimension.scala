package simulation

case class GemmDimension(m: Int, n: Int, k: Int) {

  def validate: Boolean = {
    m > 0 && n > 0 && k > 0
  }

}


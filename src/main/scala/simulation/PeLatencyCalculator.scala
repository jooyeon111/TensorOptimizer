package simulation
import scala.math.ceil
import scala.math.log10

trait PeLatencyCalculator {
  protected  def calculatePeBasicLatency(numMultiplier: Int): Int = {
    //1 Register after multiplication
    //1 output register in pe
    //ceil(log10(numMultiplier)/log10(2.0)).toInt adder tree registers
    2 + ceil(log10(numMultiplier)/log10(2.0)).toInt
  }
}

package simulation

trait BandWidthFilter {

  private def mean(data: Vector[Double]): Double =
    data.sum / data.length

  private def standardDeviation(data: Vector[Double]): Double = {
    val m = mean(data)
    math.sqrt(data.map( x=> math.pow( x - m , 2)).sum / data.length)
  }

  private def calculateScoreZ(value: Double, mean: Double, std: Double): Double =
    (value - mean) / std


  private def isWithinOneSigma(scoreZ: Double):Boolean = math.abs(scoreZ) <= 1.0

  private def isWithinTwoSigma(scoreZ: Double):Boolean = math.abs(scoreZ) <= 2.0

  private def isWithinThreeSigma(scoreZ: Double):Boolean = math.abs(scoreZ) <= 3.0

  private def calculateStats(bandwidths: Vector[Double]): (Double, Double) = {
    val m = mean(bandwidths)
    val std = standardDeviation(bandwidths)
    (m, std)
  }


  def filterConfigsWithOneSigma(configs: Vector[ArrayConfig]): Vector[ArrayConfig] = {

    val inputArrayBufferA: Vector[Double] = configs.map(_.bandwidthOfInputA.toDouble)
    val inputArrayBufferB: Vector[Double] = configs.map(_.bandwidthOfInputB.toDouble)
    val outputArrayBufferC: Vector[Double] = configs.map(_.outputBandwidth.toDouble)

    val (meanA, stdA) = calculateStats(inputArrayBufferA)
    val (meanB, stdB) = calculateStats(inputArrayBufferB)
    val (meanC, stdC) = calculateStats(outputArrayBufferC)

    configs.filter { arrayConfig =>
      val scoreA = calculateScoreZ(arrayConfig.bandwidthOfInputA, meanA, stdA)
      val scoreB = calculateScoreZ(arrayConfig.bandwidthOfInputB, meanB, stdB)
      val scoreOut = calculateScoreZ(arrayConfig.outputBandwidth, meanC, stdC)

      isWithinOneSigma(scoreA) && isWithinOneSigma(scoreB) && isWithinOneSigma(scoreOut)

    }

  }
  def filterConfigsWithTwoSigma(configs: Vector[ArrayConfig]): Vector[ArrayConfig] = {

    val inputArrayBufferA: Vector[Double] = configs.map(_.bandwidthOfInputA.toDouble)
    val inputArrayBufferB: Vector[Double] = configs.map(_.bandwidthOfInputB.toDouble)
    val outputArrayBufferC: Vector[Double] = configs.map(_.outputBandwidth.toDouble)

    val (meanA, stdA) = calculateStats(inputArrayBufferA)
    val (meanB, stdB) = calculateStats(inputArrayBufferB)
    val (meanC, stdC) = calculateStats(outputArrayBufferC)

    configs.filter { arrayConfig =>
      val scoreA = calculateScoreZ(arrayConfig.bandwidthOfInputA, meanA, stdA)
      val scoreB = calculateScoreZ(arrayConfig.bandwidthOfInputB, meanB, stdB)
      val scoreOut = calculateScoreZ(arrayConfig.outputBandwidth, meanC, stdC)

      isWithinTwoSigma(scoreA) && isWithinTwoSigma(scoreB) && isWithinTwoSigma(scoreOut)

    }

  }

  def filterConfigsWithThreeSigma(configs: Vector[ArrayConfig]): Vector[ArrayConfig] = {

    val inputArrayBufferA: Vector[Double] = configs.map(_.bandwidthOfInputA.toDouble)
    val inputArrayBufferB: Vector[Double] = configs.map(_.bandwidthOfInputB.toDouble)
    val outputArrayBufferC: Vector[Double] = configs.map(_.outputBandwidth.toDouble)

    val (meanA, stdA) = calculateStats(inputArrayBufferA)
    val (meanB, stdB) = calculateStats(inputArrayBufferB)
    val (meanC, stdC) = calculateStats(outputArrayBufferC)

    configs.filter { arrayConfig =>
      val scoreA = calculateScoreZ(arrayConfig.bandwidthOfInputA, meanA, stdA)
      val scoreB = calculateScoreZ(arrayConfig.bandwidthOfInputB, meanB, stdB)
      val scoreOut = calculateScoreZ(arrayConfig.outputBandwidth, meanC, stdC)

      isWithinThreeSigma(scoreA) && isWithinThreeSigma(scoreB) && isWithinThreeSigma(scoreOut)

    }


  }

}

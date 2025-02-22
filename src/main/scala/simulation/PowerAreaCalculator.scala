package simulation

import common.{ArrayDimension, Dataflow}

trait PowerAreaCalculator {

  val convertingToPw = 1e9

  def calculateAreaUm2(
    arrayDimension: ArrayDimension,
    dataflow: Dataflow.Value,
    streamingDimensionSize: Int
  ): Double = {
    dataflow match {
      case Dataflow.Os =>
        // Calculate OS correctly
        val groupPeRow = arrayDimension.groupPeRow
        val groupPeCol = arrayDimension.groupPeCol
        val vectorPeTotal = arrayDimension.vectorPeRow * arrayDimension.vectorPeCol
        val numMultiplier = arrayDimension.numMultiplier

        val areaUm2 = 1382.04 * (groupPeRow * groupPeCol) +
          89648.64 * (vectorPeTotal - 1) +
          69458.04 * (numMultiplier - 1) +
          3286.08 * (streamingDimensionSize - 32) / 32

        areaUm2

      case Dataflow.Is | Dataflow.Ws =>
//        Console.err.println("[error] Area calculation not available for Input Stationary dataflow")
//        sys.exit(1)
        -1
      case _ =>
        Console.err.println("[error] Invalid dataflow type")
        sys.exit(1)
    }
  }

  def calculateSwitchingPowerPj(
    arrayDimension: ArrayDimension,
    dataflow: Dataflow.Value,
    streamingDimensionSize: Int
  ): Double = {
    dataflow match {
      case Dataflow.Os =>
        // Calculate OS correctly
        val groupPeRow = arrayDimension.groupPeRow
        val groupPeCol = arrayDimension.groupPeCol
        val vectorPeTotal = arrayDimension.vectorPeRow * arrayDimension.vectorPeCol
        val numMultiplier = arrayDimension.numMultiplier

        val switchPowerMw = 0.903 * math.pow(groupPeRow / 8.0, 0.53) * math.pow(groupPeCol / 8.0, 0.53) +
          0.889 * (vectorPeTotal - 1) * math.pow((groupPeRow * groupPeCol) / (8.0 * 8.0), 0.28) +
          0.652 * (numMultiplier - 1) * (1 + 0.12 * log2(groupPeRow * groupPeCol / 64.0)) +
          0.000688 * (streamingDimensionSize - 32) * math.pow((groupPeRow * groupPeCol) / 64.0, 0.1)

        switchPowerMw * 1e9

      case Dataflow.Is | Dataflow.Ws =>
//        Console.err.println("[error] Switching power calculation not available for Input Stationary dataflow")
//        sys.exit(1)
        -1
      case _ =>
        Console.err.println("[error] Invalid dataflow type")
        sys.exit(1)
    }
  }

  def calculateInternalPowerPj(
    arrayDimension: ArrayDimension,
    dataflow: Dataflow.Value,
    streamingDimensionSize: Int
  ): Double = {
    dataflow match {
      case Dataflow.Os =>
        // Calculate OS correctly
        val groupPeRow = arrayDimension.groupPeRow
        val groupPeCol = arrayDimension.groupPeCol
        val vectorPeTotal = arrayDimension.vectorPeRow * arrayDimension.vectorPeCol
        val numMultiplier = arrayDimension.numMultiplier

        val internalPower = 26.422 * (groupPeRow * groupPeCol / 64.0) +
          25.669 * (vectorPeTotal - 1) +
          20.430 * (numMultiplier - 1) +
          0.023313 * (streamingDimensionSize - 32)* convertingToPw

        internalPower * 1e9

      case Dataflow.Is | Dataflow.Ws =>
//        Console.err.println("[error] Internal power calculation not available for Input Stationary dataflow")
//        sys.exit(1)
        -1
      case _ =>
        Console.err.println("[error] Invalid dataflow type")
        sys.exit(1)
    }
  }

  def calculateLeakagePowerPj(
    arrayDimension: ArrayDimension,
    dataflow: Dataflow.Value,
    streamingDimensionSize: Int
  ): Double = {
    dataflow match {
      case Dataflow.Os =>
        // Calculate OS correctly
        val groupPeRow = arrayDimension.groupPeRow
        val groupPeCol = arrayDimension.groupPeCol
        val vectorPeTotal = arrayDimension.vectorPeRow * arrayDimension.vectorPeCol
        val numMultiplier = arrayDimension.numMultiplier

        val leakagePowerMw = 0.389 * (groupPeRow * groupPeCol / 64.0) +
          0.394 * (vectorPeTotal - 1) +
          0.344 * (numMultiplier - 1) +
          0.000438 * (streamingDimensionSize - 32)* convertingToPw

        leakagePowerMw * 1e9

      case Dataflow.Is | Dataflow.Ws=>
//        Console.err.println("[error] Leakage power calculation not available for Input Stationary dataflow")
//        sys.exit(1)
        -1
      case _ =>
        Console.err.println("[error] Invalid dataflow type")
        sys.exit(1)
    }
  }

  private def log2(x: Double): Double = math.log(x) / math.log(2)
}
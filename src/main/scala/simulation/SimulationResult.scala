package simulation

case class SimulationResult(

  //1. Initial Workload Metrics
  totalOperationNumber: Int,
  tileSizeA: Int,
  tileSizeB: Int,
  tileSizeC: Int,
  trimTileCountA: Int,
  trimTileCountB: Int,
  singleBufferTileCapacityA: Int,
  singleBufferTileCapacityB: Int,
  singleBufferTileCapacityC: Int,

  //2 Bandwidth Info
  arrayInputBandwidthA: Int,
  arrayInputBandwidthB: Int,
  arrayOutputBandwidthC: Int,

  arrayCapacityA: Int,
  arrayCapacityB: Int,

  //3. Performance Metrics
  cycle: Long,
  arrayActiveCount: Int,

  dramReadAccessCount: Long,
  dramWriteAccessCount: Long,
  sramReadAccessCountA: Long,
  sramWriteAccessCountA: Long,
  sramReadAccessCountB: Long,
  sramWriteAccessCountB: Long,

  dramHitRatio: Double,
  dramMissRatio: Double,

  sramHitRatioA: Double,
  sramHitRatioB: Double,
  sramMissRatioA: Double,
  sramMissRatioB: Double,
  sramHitRatio: Double,
  sramMissRatio: Double,

  //3. Pipeline State
  dramStallCount: Int,

  firstFillUpCycleA: Long,
  bufferSwapCountA: Int,
  bufferSwapStallCountA: Int,

  firstFillUpCycleB: Long,
  bufferSwapCountB: Int,
  bufferSwapStallCountB: Int,

  firstFillUpCycleC: Long,
  bufferSwapCountC: Int,
  bufferSwapStallCountC: Int,

  //4. Memory Utilization
  averageMemoryUsageKbA: Double,
  averageMemoryUtilizationA: Double,

  averageMemoryUsageKbB: Double,
  averageMemoryUtilizationB: Double,

  averageMemoryUsageKbC: Double,
  averageMemoryUtilizationC: Double,

  //5. Energy
  sramReadEnergyPjA: Option[Double],
  sramWriteEnergyPjA: Option[Double],
  sramLeakageEnergyPjA: Option[Double],
  sramEnergyPjA: Option[Double],

  sramReadEnergyPjB: Option[Double],
  sramWriteEnergyPjB: Option[Double],
  sramLeakageEnergyPjB: Option[Double],
  sramEnergyPjB: Option[Double],

  sramReadEnergyPjC: Option[Double],
  sramWriteEnergyPjC: Option[Double],
  sramLeakageEnergyPjC: Option[Double],
  sramEnergyPjC: Option[Double],

  dramReadEnergyPj: Option[Double],
  dramWriteEnergyPj: Option[Double],
  dramEnergyPj: Option[Double],

  arrayDynamicEnergyPj: Option[Double],
  arrayLeakageEnergyPj: Option[Double],
  arrayEnergy: Option[Double],

  energyPj: Option[Double],

  //6. Area
  sramAreaMmA: Option[Double],
  sramAreaMmB: Option[Double],
  sramAreaMmC: Option[Double],
  arrayAreaMm: Option[Double],
  areaMm: Option[Double],

) extends Logger {

  def printFullResults(loggerOption: LoggerOption): Unit = {
    setMode(loggerOption)
    log(s"[Simulation Results]")
    log("\t[Workload Metrics]")
    log(s"")
    log(s"\t\tTile Size A: $tileSizeA")
    log(s"\t\tTile Size B: $tileSizeB")
    log(s"\t\tTile Size C: $tileSizeC")
    log(s"\t\tTile A Trim Count: $trimTileCountA")
    log(s"\t\tTile B Trim Count: $trimTileCountB")
    log(s"")
    log("\t[Bandwidth Information]")
    log(s"\t\tArray Input Bandwidth A: $arrayInputBandwidthA bit")
    log(s"\t\tArray Input Bandwidth B: $arrayInputBandwidthB bit")
    log(s"\t\tArray Output Bandwidth C: $arrayOutputBandwidthC bit")
    log(s"")
    log(s"\t\tArray Capacity A: $arrayCapacityA bit")
    log(s"\t\tArray Capacity B: $arrayCapacityB bit")
    log(s"")
    log("\t[Performance Metrics]")
    log(s"")
    log(s"\t\tTotal Cycle: $cycle")
    log(s"\t\tArray Active Count: $arrayActiveCount")
    log(s"")
    log(s"\t\tDRAM Read Access Count: $dramReadAccessCount")
    log(s"\t\tDRAM Write Access Count: $dramWriteAccessCount")
    log(s"\t\tSRAM A Read Access Count: $sramReadAccessCountA")
    log(s"\t\tSRAM A Write Access Count: $sramWriteAccessCountA")
    log(s"\t\tSRAM B Read Access Count: $sramReadAccessCountB")
    log(s"\t\tSRAM B Write Access Count: $sramWriteAccessCountB")
    log(s"")
    log(s"\t\tDRAM Hit Ratio: ${String.format("%.2f", dramHitRatio)} %")
    log(s"\t\tDRAM Miss Ratio: ${String.format("%.2f", dramMissRatio)} %")
    log(s"")
    log(s"\t\tSRAM A Hit Ratio: ${String.format("%.2f", sramHitRatioA)} %")
    log(s"\t\tSRAM B Hit Ratio: ${String.format("%.2f", sramHitRatioB)} %")
    log(s"\t\tSRAM A Miss Ratio: ${String.format("%.2f", sramMissRatioA)} %")
    log(s"\t\tSRAM B Miss Ratio: ${String.format("%.2f", sramMissRatioB)} %")
    log(s"\t\tSRAM Hit Ratio: ${String.format("%.2f", sramHitRatio)} %")
    log(s"\t\tSRAM Miss Ratio: ${String.format("%.2f", sramMissRatio)} %")
    log(s"")
    log("\t[Pipeline State]")
    log(s"")
    log(s"\t\tDRAM Stall Cycle: $dramStallCount")
    log(s"")
    log(s"\t\tSRAM A First Fill Up Cycle: $firstFillUpCycleA")
    log(s"\t\tSRAM A Buffer swap Success Count: $bufferSwapCountA")
    log(s"\t\tSRAM A Buffer Swap Stall Count: $bufferSwapStallCountA")
    log(s"")
    log(s"\t\tSRAM B First Fill Up Cycle: $firstFillUpCycleB")
    log(s"\t\tSRAM B Buffer swap Success Count: $bufferSwapCountB")
    log(s"\t\tSRAM B Buffer Swap Stall Count: $bufferSwapStallCountB")
    log(s"")
    log(s"\t\tSRAM C First Fill Up Cycle: $firstFillUpCycleC")
    log(s"\t\tSRAM C Buffer swap Success Count: $bufferSwapCountC")
    log(s"\t\tSRAM C Buffer Swap Stall Count: $bufferSwapStallCountC")
    log(s"")
    log("\t[Memory Utilization]")
    log(s"")
    log(s"\t\tSRAM A Average Memory Usage: ${String.format("%.2f", averageMemoryUsageKbA)} KB")
    log(s"\t\tSRAM A Average Memory Utilization: ${String.format("%.2f", averageMemoryUtilizationA)} %")
    log(s"\t\tSRAM B Average Memory Usage: ${String.format("%.2f", averageMemoryUsageKbB)} KB")
    log(s"\t\tSRAM B Average Memory Utilization: ${String.format("%.2f", averageMemoryUtilizationB)} %")
    log(s"\t\tSRAM C Average Memory Usage: ${String.format("%.2f", averageMemoryUsageKbC)} KB")
    log(s"\t\tSRAM C Average Memory Utilization: ${String.format("%.2f", averageMemoryUtilizationC)} %")
    log(s"")

    log(s"\t\tSRAM A First Fill Up Cycle: $firstFillUpCycleA")
    log(s"\t\tSRAM A Buffer swap Success Count: $bufferSwapCountA")
    log(s"\t\tSRAM A Buffer Swap Stall Count: $bufferSwapStallCountA")
    log(s"")
    log(s"\t\tSRAM B First Fill Up Cycle: $firstFillUpCycleB")
    log(s"\t\tSRAM B Buffer swap Success Count: $bufferSwapCountB")
    log(s"\t\tSRAM B Buffer Swap Stall Count: $bufferSwapStallCountB")
    log(s"")
    log(s"\t\tSRAM C First Fill Up Cycle: $firstFillUpCycleC")
    log(s"\t\tSRAM C Buffer swap Success Count: $bufferSwapCountC")
    log(s"\t\tSRAM C Buffer Swap Stall Count: $bufferSwapStallCountC")
    log(s"")
    log("\t[Memory Utilization]")
    log(s"")
    log(s"\t\tSRAM A Average Memory Usage: ${String.format("%.2f", averageMemoryUsageKbA)} KB")
    log(s"\t\tSRAM A Average Memory Utilization: ${String.format("%.2f", averageMemoryUtilizationA)} %")
    log(s"\t\tSRAM B Average Memory Usage: ${String.format("%.2f", averageMemoryUsageKbB)} KB")
    log(s"\t\tSRAM B Average Memory Utilization: ${String.format("%.2f", averageMemoryUtilizationB)} %")
    log(s"\t\tSRAM C Average Memory Usage: ${String.format("%.2f", averageMemoryUsageKbC)} KB")
    log(s"\t\tSRAM C Average Memory Utilization: ${String.format("%.2f", averageMemoryUtilizationC)} %")
    log(s"")

    if (isEnergyReportValid) {
      log("\t[Energy Analysis (pJ)]")
      log(s"\t\tSRAM A Read Energy: ${String.format("%.2f", sramReadEnergyPjA.get)} pJ")
      log(s"\t\tSRAM A Write Energy: ${String.format("%.2f", sramWriteEnergyPjA.get)} pJ")
      log(s"\t\tSRAM A Leakage Energy: ${String.format("%.2f", sramLeakageEnergyPjA.get)} pJ")
      log(s"\t\tSRAM A Total Energy: ${String.format("%.2f", sramEnergyPjA.get)} pJ")
      log("")

      log(s"\t\tSRAM B Read Energy: ${String.format("%.2f", sramReadEnergyPjB.get)} pJ")
      log(s"\t\tSRAM B Write Energy: ${String.format("%.2f", sramWriteEnergyPjB.get)} pJ")
      log(s"\t\tSRAM B Leakage Energy: ${String.format("%.2f", sramLeakageEnergyPjB.get)} pJ")
      log(s"\t\tSRAM B Total Energy: ${String.format("%.2f", sramEnergyPjB.get)} pJ")
      log("")

      log(s"\t\tSRAM C Read Energy: ${String.format("%.2f", sramReadEnergyPjC.get)} pJ")
      log(s"\t\tSRAM C Write Energy: ${String.format("%.2f", sramWriteEnergyPjC.get)} pJ")
      log(s"\t\tSRAM C Leakage Energy: ${String.format("%.2f", sramLeakageEnergyPjC.get)} pJ")
      log(s"\t\tSRAM C Total Energy: ${String.format("%.2f", sramEnergyPjC.get)} pJ")
      log("")

      log(s"\t\tDRAM Read Energy: ${String.format("%.2f", dramReadEnergyPj.get)} pJ")
      log(s"\t\tDRAM Write Energy: ${String.format("%.2f", dramWriteEnergyPj.get)} pJ")
      log(s"\t\tDRAM Total Energy: ${String.format("%.2f", dramEnergyPj.get)} pJ")
      log("")

      log(s"\t\tArray Dynamic Energy: ${String.format("%.2f", arrayDynamicEnergyPj.get)} pJ")
      log(s"\t\tArray Leakage Energy: ${String.format("%.2f", arrayLeakageEnergyPj.get)} pJ")
      log(s"\t\tArray Total Energy: ${String.format("%.2f", arrayEnergy.get)} pJ")
      log("")

      log(s"\t\tTotal Energy: ${String.format("%.2f", energyPj.get)} pJ")
      log("")
    }

    // Area Analysis
    if (isAreaReportValid) {
      log("\t[Area Analysis (mm^2)]")
      log("")
      log(s"\t\tSRAM A Area: ${String.format("%.2f", sramAreaMmA.get)} mm^2")
      log(s"\t\tSRAM B Area: ${String.format("%.2f", sramAreaMmB.get)} mm^2")
      log(s"\t\tSRAM C Area: ${String.format("%.2f", sramAreaMmC.get)} mm^2")
      log(s"\t\tArray Area: ${String.format("%.2f", arrayAreaMm.get)} mm^2")
      log(s"\t\tTotal Area: ${String.format("%.2f", areaMm.get)} mm^2")
    }

    log(s"")

    log(s"")
  }

  def showSummary(loggerOption: LoggerOption, arrayConfigString: String): Unit = {
    setMode(loggerOption)

//    if(sramReadEnergyPjA.isDefined) {
//      println("SRAM READ Energy PJ A is Valid")
//    } else {
//      println("SRAM READ Energy PJ A is not Valid")
//    }
//
//    if(sramWriteEnergyPjA.isDefined) {
//      println("SRAM Write Energy PJ A is Valid")
//    } else {
//      println("SRAM Write Energy PJ A is not Valid")
//    }
//
//    if(sramLeakageEnergyPjA.isDefined) {
//      println("SRAM Leakage Energy PJ A is Valid")
//    } else {
//      println("SRAM Leakage Energy PJ A is not Valid")
//    }
//
//    if(sramEnergyPjA.isDefined) {
//      println("SRAM Total Energy PJ A is Valid")
//    } else {
//      println("SRAM Total Energy PJ A is not Valid")
//    }
//
//    if(sramReadEnergyPjB.isDefined) {
//      println("SRAM READ Energy PJ B is Valid")
//    } else {
//      println("SRAM READ Energy PJ B is not Valid")
//    }
//
//    if(sramWriteEnergyPjB.isDefined) {
//      println("SRAM Write Energy PJ B is Valid")
//    } else {
//      println("SRAM Write Energy PJ B is not Valid")
//    }
//
//    if(sramLeakageEnergyPjB.isDefined) {
//      println("SRAM Leakage Energy PJ B is Valid")
//    } else {
//      println("SRAM Leakage Energy PJ B is not Valid")
//    }
//
//    if(sramEnergyPjB.isDefined) {
//      println("SRAM Total Energy PJ B is Valid")
//    } else {
//      println("SRAM Total Energy PJ B is not Valid")
//    }
//
//    if(sramReadEnergyPjC.isDefined) {
//      println("SRAM READ Energy PJ C is Valid")
//    } else {
//      println("SRAM READ Energy PJ C is not Valid")
//    }
//
//    if(sramWriteEnergyPjC.isDefined) {
//      println("SRAM Write Energy PJ C is Valid")
//    } else {
//      println("SRAM Write Energy PJ C is not Valid")
//    }
//
//    if(sramLeakageEnergyPjC.isDefined) {
//      println("SRAM Leakage Energy PJ C is Valid")
//    } else {
//      println("SRAM Leakage Energy PJ C is not Valid")
//    }
//
//    if(sramEnergyPjC.isDefined) {
//      println("SRAM Total Energy PJ C is Valid")
//    } else {
//      println("SRAM Total Energy PJ C is not Valid")
//    }
//
//    if(dramReadEnergyPj.isDefined) {
//      println("DRAM Read Energy PJ is Valid")
//    } else {
//      println("DRAM Read Energy PJ is not Valid")
//    }
//
//    if(dramWriteEnergyPj.isDefined) {
//      println("DRAM Write Energy PJ is Valid")
//    } else {
//      println("DRAM Write Energy PJ is not Valid")
//    }
//
//    if(dramEnergyPj.isDefined) {
//      println("DRAM Total Energy PJ is Valid")
//    } else {
//      println("DRAM Total Energy PJ is not Valid")
//    }
//
//    if(arrayDynamicEnergyPj.isDefined) {
//      println("Array Dynamic Energy PJ is Valid")
//    } else {
//      println("Array Dynamic Energy PJ is not Valid")
//    }
//
//    if(arrayLeakageEnergyPj.isDefined) {
//      println("Array Leakage Energy PJ is Valid")
//    } else {
//      println("Array Leakage Energy PJ is not Valid")
//    }
//
//    if(arrayEnergy.isDefined) {
//      println("Array Total Energy PJ is Valid")
//    } else {
//      println("Array Total Energy PJ is not Valid")
//    }
//
//    if(energyPj.isDefined) {
//      println("Total Energy PJ is Valid")
//    } else {
//      println("Total Energy PJ is not Valid")
//    }
//
//    if(sramAreaMmA.isDefined) {
//      println("SRAM Area MM A is Valid")
//    } else {
//      println("SRAM Area MM A is not Valid")
//    }
//
//    if(sramAreaMmB.isDefined) {
//      println("SRAM Area MM B is Valid")
//    } else {
//      println("SRAM Area MM B is not Valid")
//    }
//
//    if(sramAreaMmC.isDefined) {
//      println("SRAM Area MM C is Valid")
//    } else {
//      println("SRAM Area MM C is not Valid")
//    }
//
//    if(arrayAreaMm.isDefined) {
//      println("Array Area MM is Valid")
//    } else {
//      println("Array Area MM is not Valid")
//    }
//
//    if(areaMm.isDefined) {
//      println("Total Area MM is Valid")
//    } else {
//      println("Total Area MM is not Valid")
//    }


    if(isEnergyReportValid && isAreaReportValid){
      log(s"\t[$arrayConfigString] Cycle: $cycle," +
        s" Area: ${String.format("%.2f", arrayAreaMm.get)} mm^2," +
        s" Energy: ${String.format("%.2f", energyPj.get)} pJ")
    } else {
      log(s"\tCycle: $cycle")
    }

  }

  private def isEnergyReportValid: Boolean = {
    sramReadEnergyPjA.isDefined &&
      sramWriteEnergyPjA.isDefined &&
      sramLeakageEnergyPjA.isDefined &&
      sramEnergyPjA.isDefined &&
      sramReadEnergyPjB.isDefined &&
      sramWriteEnergyPjB.isDefined &&
      sramLeakageEnergyPjB.isDefined &&
      sramEnergyPjB.isDefined &&
      sramReadEnergyPjC.isDefined &&
      sramWriteEnergyPjC.isDefined &&
      sramLeakageEnergyPjC.isDefined &&
      sramEnergyPjC.isDefined &&
      dramReadEnergyPj.isDefined &&
      dramWriteEnergyPj.isDefined &&
      dramEnergyPj.isDefined &&
      arrayDynamicEnergyPj.isDefined &&
      arrayLeakageEnergyPj.isDefined &&
      arrayEnergy.isDefined &&
      energyPj.isDefined
  }

  private def isAreaReportValid: Boolean = {
    sramAreaMmA.isDefined &&
      sramAreaMmB.isDefined &&
      sramAreaMmC.isDefined &&
      arrayAreaMm.isDefined &&
      areaMm.isDefined
  }

}

object SimulationResult {
  private val defaultErrorValue = -1
  private val defaultErrorValueLong = Long.MaxValue
  private val defaultErrorValueDouble = -1.0

  private case class DefaultValues(
    intValue: Int = defaultErrorValue,
    longValue: Long = defaultErrorValueLong,
    doubleValue: Double = defaultErrorValueDouble
  )

  def apply(wrongCycle: Long): SimulationResult = {
    val defaults = DefaultValues()

    SimulationResult(
      // 1. Initial Workload Metrics
      totalOperationNumber = defaults.intValue,
      tileSizeA = defaults.intValue,
      tileSizeB = defaults.intValue,
      tileSizeC = defaults.intValue,
      trimTileCountA = defaults.intValue,
      trimTileCountB = defaults.intValue,
      singleBufferTileCapacityA = defaults.intValue,
      singleBufferTileCapacityB = defaults.intValue,
      singleBufferTileCapacityC = defaults.intValue,

      // 2. Bandwidth Info
      arrayInputBandwidthA = defaults.intValue,
      arrayInputBandwidthB = defaults.intValue,
      arrayOutputBandwidthC = defaults.intValue,

      arrayCapacityA = defaults.intValue,
      arrayCapacityB = defaults.intValue,

      // 3. Performance Metrics
      cycle = wrongCycle,
      arrayActiveCount = defaults.intValue,

      // 4. Memory Access Logs
      dramReadAccessCount = defaults.longValue,
      dramWriteAccessCount = defaults.longValue,
      sramReadAccessCountA = defaults.longValue,
      sramWriteAccessCountA = defaults.longValue,
      sramReadAccessCountB = defaults.longValue,
      sramWriteAccessCountB = defaults.longValue,

      // 5. Hit/Miss Ratios
      dramHitRatio = defaults.doubleValue,
      dramMissRatio = defaults.doubleValue,

      sramHitRatioA = defaults.doubleValue,
      sramHitRatioB = defaults.doubleValue,
      sramMissRatioA = defaults.doubleValue,
      sramMissRatioB = defaults.doubleValue,
      sramHitRatio = defaults.doubleValue,
      sramMissRatio = defaults.doubleValue,

      // 6. Buffer State
      dramStallCount = defaults.intValue,

      firstFillUpCycleA = defaults.longValue,
      bufferSwapCountA = defaults.intValue,
      bufferSwapStallCountA = defaults.intValue,

      firstFillUpCycleB = defaults.longValue,
      bufferSwapCountB = defaults.intValue,
      bufferSwapStallCountB = defaults.intValue,

      firstFillUpCycleC = defaults.longValue,
      bufferSwapCountC = defaults.intValue,
      bufferSwapStallCountC = defaults.intValue,

      // 7. Memory Usage Stats
      averageMemoryUsageKbA = defaults.doubleValue,
      averageMemoryUtilizationA = defaults.doubleValue,

      averageMemoryUsageKbB = defaults.doubleValue,
      averageMemoryUtilizationB = defaults.doubleValue,

      averageMemoryUsageKbC = defaults.doubleValue,
      averageMemoryUtilizationC = defaults.doubleValue,
      sramReadEnergyPjA = None,
      sramWriteEnergyPjA = None,
      sramLeakageEnergyPjA = None,
      sramEnergyPjA = None,

      sramReadEnergyPjB = None,
      sramWriteEnergyPjB = None,
      sramLeakageEnergyPjB = None,
      sramEnergyPjB = None,

      sramReadEnergyPjC = None,
      sramWriteEnergyPjC = None,
      sramLeakageEnergyPjC = None,
      sramEnergyPjC = None,

      dramReadEnergyPj = None,
      dramWriteEnergyPj = None,
      dramEnergyPj = None,

      arrayDynamicEnergyPj = None,
      arrayLeakageEnergyPj = None,
      arrayEnergy = None,

      energyPj = None,

      //6. Area
      sramAreaMmA = None,
      sramAreaMmB = None,
      sramAreaMmC = None,
      arrayAreaMm = None,
      areaMm = None,
    )
  }
}
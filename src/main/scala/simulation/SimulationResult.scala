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

  //4. Pipeline State
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

  //5. Memory Utilization
  averageMemoryUsageKbA: Double,
  averageMemoryUtilizationA: Double,

  averageMemoryUsageKbB: Double,
  averageMemoryUtilizationB: Double,

  averageMemoryUsageKbC: Double,
  averageMemoryUtilizationC: Double,

  //6. Reference Data
  dramReferenceData: Option[DramReferenceData],
  sramReferenceDataTable: Option[SramDataTable],
  arraySynthesisData: Option[ArraySynthesisData],

  //7. Energy
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

  arrayDynamicEnergyPj: Double,
  arrayLeakageEnergyPj: Double,
  arrayEnergy: Double,

  energyPj: Option[Double],

  //8. Area
  sramAreaUm2A: Option[Double],
  sramAreaUm2B: Option[Double],
  sramAreaUm2C: Option[Double],
  arrayAreaUm2: Double,
  areaUm2: Option[Double],

) extends Logger {

  def printFullResults(loggerOption: LoggerOption): Unit = {
    setMode(loggerOption)
    log(s"[Simulation Results]")
    log("\t[Workload Metrics]")
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
    log(s"\t\tSRAM A Average Memory Usage: ${String.format("%.2f", averageMemoryUsageKbA)} KB")
    log(s"\t\tSRAM A Average Memory Utilization: ${String.format("%.2f", averageMemoryUtilizationA)} %")
    log(s"\t\tSRAM B Average Memory Usage: ${String.format("%.2f", averageMemoryUsageKbB)} KB")
    log(s"\t\tSRAM B Average Memory Utilization: ${String.format("%.2f", averageMemoryUtilizationB)} %")
    log(s"\t\tSRAM C Average Memory Usage: ${String.format("%.2f", averageMemoryUsageKbC)} KB")
    log(s"\t\tSRAM C Average Memory Utilization: ${String.format("%.2f", averageMemoryUtilizationC)} %")
    log(s"")

    if(isReferenceDataValid){
      log(s"\t[DRAM Reference Data]")
      log(s"\t\tDRAM Read Energy per Access: ${String.format("%.2f", dramReferenceData.get.readEnergyPj)} pJ ")
      log(s"\t\tDRAM Read Energy per Access: ${String.format("%.2f", dramReferenceData.get.writeEnergyPj)} pJ ")
      log(s"")
      log(s"\t[SRAM Reference Data]")
      log(s"\t\t[Input SRAM A]")
      log(s"\t\tCapacity: ${sramReferenceDataTable.get.sramA.get.capacityKb} KB")
      log(s"\t\tBandwidth bit: ${sramReferenceDataTable.get.sramA.get.bandwidthBits}")
      log(s"\t\tRead Energy Per Access: ${sramReferenceDataTable.get.sramA.get.readEnergyPj} pJ")
      log(s"\t\tWrite Energy Per Access: ${sramReferenceDataTable.get.sramA.get.writeEnergyPj} pJ")
      log(s"\t\tLeakage Power: ${sramReferenceDataTable.get.sramA.get.leakagePowerPw} pW")
      log(s"\t\tArea: ${sramReferenceDataTable.get.sramA.get.areaUm2} pJ")
      log(s"")
      log(s"\t\t[Weight SRAM B]")
      log(s"\t\tCapacity: ${sramReferenceDataTable.get.sramB.get.capacityKb} KB")
      log(s"\t\tBandwidth bit: ${sramReferenceDataTable.get.sramB.get.bandwidthBits}")
      log(s"\t\tRead Energy Per Access: ${sramReferenceDataTable.get.sramB.get.readEnergyPj} pJ")
      log(s"\t\tWrite Energy Per Access: ${sramReferenceDataTable.get.sramB.get.writeEnergyPj} pJ")
      log(s"\t\tLeakage Power: ${sramReferenceDataTable.get.sramB.get.leakagePowerPw} pW")
      log(s"\t\tArea: ${sramReferenceDataTable.get.sramB.get.areaUm2} pJ")
      log(s"")
      log(s"\t\t[Output SRAM C]")
      log(s"\t\tCapacity: ${sramReferenceDataTable.get.sramC.get.capacityKb} KB")
      log(s"\t\tBandwidth bit: ${sramReferenceDataTable.get.sramC.get.bandwidthBits}")
      log(s"\t\tRead Energy Per Access: ${sramReferenceDataTable.get.sramC.get.readEnergyPj} pJ")
      log(s"\t\tWrite Energy Per Access: ${sramReferenceDataTable.get.sramC.get.writeEnergyPj} pJ")
      log(s"\t\tLeakage Power: ${sramReferenceDataTable.get.sramC.get.leakagePowerPw} pW")
      log(s"\t\tArea: ${sramReferenceDataTable.get.sramC.get.areaUm2} pJ")
      log(s"")
    }

    if (isEnergyReportValid) {
      log("\t[Energy Analysis (pJ)]")

      log("\t\t[DRAM]")
      log(s"\t\tDRAM Read Energy: ${String.format("%.2f", dramReadEnergyPj.get)} pJ")
      log(s"\t\tDRAM Write Energy: ${String.format("%.2f", dramWriteEnergyPj.get)} pJ")
      log(s"\t\tDRAM Total Energy: ${String.format("%.2f", dramEnergyPj.get)} pJ")
      log("")

      log("\t\t[Input SRAM A]")
      log(s"\t\tRead Energy: ${String.format("%.2f", sramReadEnergyPjA.get)} pJ")
      log(s"\t\tWrite Energy: ${String.format("%.2f", sramWriteEnergyPjA.get)} pJ")
      log(s"\t\tLeakage Energy: ${String.format("%.2f", sramLeakageEnergyPjA.get)} pJ")
      log(s"\t\tTotal Energy: ${String.format("%.2f", sramEnergyPjA.get)} pJ")
      log("")

      log("\t\t[Weight SRAM B]")
      log(s"\t\tRead Energy: ${String.format("%.2f", sramReadEnergyPjB.get)} pJ")
      log(s"\t\tWrite Energy: ${String.format("%.2f", sramWriteEnergyPjB.get)} pJ")
      log(s"\t\tLeakage Energy: ${String.format("%.2f", sramLeakageEnergyPjB.get)} pJ")
      log(s"\t\tTotal Energy: ${String.format("%.2f", sramEnergyPjB.get)} pJ")
      log("")

      log("\t\t[Output SRAM C]")
      log(s"\t\tRead Energy: ${String.format("%.2f", sramReadEnergyPjC.get)} pJ")
      log(s"\t\tWrite Energy: ${String.format("%.2f", sramWriteEnergyPjC.get)} pJ")
      log(s"\t\tLeakage Energy: ${String.format("%.2f", sramLeakageEnergyPjC.get)} pJ")
      log(s"\t\tTotal Energy: ${String.format("%.2f", sramEnergyPjC.get)} pJ")
      log("")

      log("\t\t[Array Energy]")
      log(s"\t\tEnergy: ${String.format("%.2f", arrayDynamicEnergyPj)} pJ")
      log(s"\t\tEnergy: ${String.format("%.2f", arrayLeakageEnergyPj)} pJ")
      log(s"\t\tEnergy: ${String.format("%.2f", arrayEnergy)} pJ")
      log("")

      log("\t\t[Total Energy]")
      log(s"\t\t${String.format("%.2f", energyPj.get)} pJ")
      log("")
      log(s"\t\t[CSV Format (DRAM, SRAM A, SRAM B, SRAM C, ARRAY)] Energy")
      log(s"\t\t${String.format("%.2f", dramEnergyPj.get)}, " +
        s"${String.format("%.2f", sramEnergyPjA.get)}, " +
        s"${String.format("%.2f", sramEnergyPjB.get)}, " +
        s"${String.format("%.2f", sramEnergyPjC.get)}, " +
        s"${String.format("%.2f", arrayEnergy)}"
      )
    }

    // Area Analysis
    if (isAreaReportValid) {
      log("\t[Area Analysis (um^2)]")
      log("")
      log(s"\t\tSRAM A Area: ${String.format("%.2f", sramAreaUm2A.get)} um^2")
      log(s"\t\tSRAM B Area: ${String.format("%.2f", sramAreaUm2B.get)} um^2")
      log(s"\t\tSRAM C Area: ${String.format("%.2f", sramAreaUm2C.get)} um^2")
      log(s"\t\tArray Area: ${String.format("%.2f", arrayAreaUm2)} um^2")
      log(s"\t\tTotal Area: ${String.format("%.2f", areaUm2.get)} um^2")
      log(s"")
      log(s"\t\t[CSV Format (SRAM A, SRAM B, SRAM C, ARRAY)] Area")
      log(s"\t\t${String.format("%.2f", sramAreaUm2A.get)}, " +
        s"${String.format("%.2f", sramAreaUm2B.get)}, " +
        s"${String.format("%.2f", sramAreaUm2C.get)}, " +
        s"${String.format("%.2f", sramAreaUm2C.get)}"
      )
    }

    log(s"")
    log(s"")
  }

  private def isReferenceDataValid: Boolean = {
    dramReferenceData.isDefined && sramReferenceDataTable.isDefined && dramReferenceData.isDefined
  }

  def isEnergyReportValid: Boolean = {
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
      energyPj.isDefined
  }

  def isAreaReportValid: Boolean = {
    sramAreaUm2A.isDefined &&
      sramAreaUm2B.isDefined &&
      sramAreaUm2C.isDefined &&
      areaUm2.isDefined
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

      dramReadAccessCount = defaults.longValue,
      dramWriteAccessCount = defaults.longValue,
      sramReadAccessCountA = defaults.longValue,
      sramWriteAccessCountA = defaults.longValue,
      sramReadAccessCountB = defaults.longValue,
      sramWriteAccessCountB = defaults.longValue,

      dramHitRatio = defaults.doubleValue,
      dramMissRatio = defaults.doubleValue,

      sramHitRatioA = defaults.doubleValue,
      sramHitRatioB = defaults.doubleValue,
      sramMissRatioA = defaults.doubleValue,
      sramMissRatioB = defaults.doubleValue,
      sramHitRatio = defaults.doubleValue,
      sramMissRatio = defaults.doubleValue,

      // 4. Pipeline state
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

      // 5. Memory Utilization
      averageMemoryUsageKbA = defaults.doubleValue,
      averageMemoryUtilizationA = defaults.doubleValue,

      averageMemoryUsageKbB = defaults.doubleValue,
      averageMemoryUtilizationB = defaults.doubleValue,

      averageMemoryUsageKbC = defaults.doubleValue,
      averageMemoryUtilizationC = defaults.doubleValue,

      //6. Reference Data
      dramReferenceData = None,
      sramReferenceDataTable = None,
      arraySynthesisData = None,

      //7, Energy Report
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

      arrayDynamicEnergyPj = defaults.doubleValue,
      arrayLeakageEnergyPj = defaults.doubleValue,
      arrayEnergy = defaults.doubleValue,

      energyPj = None,

      //8. Area
      sramAreaUm2A = None,
      sramAreaUm2B = None,
      sramAreaUm2C = None,
      arrayAreaUm2 = defaults.doubleValue,
      areaUm2 = None,
    )
  }
}
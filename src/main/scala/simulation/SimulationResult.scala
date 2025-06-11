package simulation

case class SimulationResult(

  //1. Initial Workload Metrics
  totalOperationNumber: Int,
  tileSizeA: Int,
  tileSizeB: Int,
  tileSizeC: Int,
  skipTileCountA: Int,
  skipTileCountB: Int,
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

  offChipMemoryReadAccessCount: Long,
  offChipMemoryWriteAccessCount: Long,
  sramReadAccessCountA: Long,
  sramWriteAccessCountA: Long,
  sramReadAccessCountB: Long,
  sramWriteAccessCountB: Long,

  offChipMemoryHitRatio: Double,
  offChipMemoryMissRatio: Double,

  sramHitRatioA: Double,
  sramHitRatioB: Double,
  sramMissRatioA: Double,
  sramMissRatioB: Double,
  sramHitRatio: Double,
  sramMissRatio: Double,

  //4. Pipeline State
  offChipMemoryStallCount: Int,

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

  averageMemoryUtilization: Double,

  //6. Reference Data
  offChipMemoryReferenceData: Option[OffChipMemoryReferenceData],
  sramModelDataTable: Option[DividedSramModelTable],
  arraySynthesisSource: Option[ArraySynthesisSource.Value],
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

  offChipMemoryReadEnergyPj: Option[Double],
  offChipMemoryWriteEnergyPj: Option[Double],
  offChipMemoryEnergyPj: Option[Double],

  arrayDynamicEnergyPj: Option[Double],
  arrayLeakageEnergyPj: Option[Double],
  arrayEnergy: Option[Double],

  energyPj: Option[Double],

  //8. Area
  sramAreaUm2A: Option[Double],
  sramAreaUm2B: Option[Double],
  sramAreaUm2C: Option[Double],
  arrayAreaUm2: Option[Double],
  areaUm2: Option[Double],

  edap: Option[Double],

) extends Logger {

  def printFullResults(loggerOption: LoggerOption): Unit = {
    setMode(loggerOption)
    log(s"[Simulation Results]")
    log("\t[Workload Metrics]")
    log(s"\t\tTile Size A: $tileSizeA")
    log(s"\t\tTile Size B: $tileSizeB")
    log(s"\t\tTile Size C: $tileSizeC")
    log(s"\t\tTile A Skip Count: $skipTileCountA")
    log(s"\t\tTile B Skip Count: $skipTileCountB")
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
    log(s"\t\tOff Chip Memory Read Access Count: $offChipMemoryReadAccessCount")
    log(s"\t\tOff Chip Memory Write Access Count: $offChipMemoryWriteAccessCount")
    log(s"\t\tSRAM A Read Access Count: $sramReadAccessCountA")
    log(s"\t\tSRAM A Write Access Count: $sramWriteAccessCountA")
    log(s"\t\tSRAM B Read Access Count: $sramReadAccessCountB")
    log(s"\t\tSRAM B Write Access Count: $sramWriteAccessCountB")
    log(s"")
    log(s"\t\tOff Chip Memory Hit Ratio: ${String.format("%.2f", offChipMemoryHitRatio)} %")
    log(s"\t\tOff Chip Memory Miss Ratio: ${String.format("%.2f", offChipMemoryMissRatio)} %")
    log(s"")
    log(s"\t\tSRAM A Hit Ratio: ${String.format("%.2f", sramHitRatioA)} %")
    log(s"\t\tSRAM B Hit Ratio: ${String.format("%.2f", sramHitRatioB)} %")
    log(s"\t\tSRAM A Miss Ratio: ${String.format("%.2f", sramMissRatioA)} %")
    log(s"\t\tSRAM B Miss Ratio: ${String.format("%.2f", sramMissRatioB)} %")
    log(s"\t\tSRAM Hit Ratio: ${String.format("%.2f", sramHitRatio)} %")
    log(s"\t\tSRAM Miss Ratio: ${String.format("%.2f", sramMissRatio)} %")
    log(s"")
    log("\t[Pipeline State]")
    log(s"\t\tOff Chip Memory Stall Cycle: $offChipMemoryStallCount")
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
    log(s"\t\tSRAM Average Memory Utilization: ${String.format("%.2f", averageMemoryUtilization)} %")
    log(s"")
    log(s"\t[SRAM Tile Capacity Information]")
    log(s"\t\tSRAM A: $singleBufferTileCapacityA tiles per buffer (${singleBufferTileCapacityA * 2} tiles total)")
    log(s"\t\tSRAM B: $singleBufferTileCapacityB tiles per buffer (${singleBufferTileCapacityB * 2} tiles total)")
    log(s"\t\tSRAM C: $singleBufferTileCapacityC tiles per buffer (${singleBufferTileCapacityC * 2} tiles total)")
    log(s"")

    if(isReferenceDataValid){
      log(s"\t[Off Chip Memory Data from DRAMSim3]")
      log(s"\t\tOff Chip Memory Read Energy per Access: ${String.format("%.2f", offChipMemoryReferenceData.get.readEnergyPj)} pJ ")
      log(s"\t\tOff Chip Memory Read Energy per Access: ${String.format("%.2f", offChipMemoryReferenceData.get.writeEnergyPj)} pJ ")
      log(s"")
      log(s"\t[SRAM Data from NVSim]")
      log(s"\t\t[Input SRAM A]")
      log(s"\t\t\tBank Count: ${sramModelDataTable.get.sramA.bankCount}")
      log(s"\t\t\tBank Capacity: ${sramModelDataTable.get.sramA.referenceData.capacityKb} KB")
      log(s"\t\t\tBank Bandwidth: ${sramModelDataTable.get.sramA.referenceData.bandwidthBits}")
      log(s"\t\t\tBank Read Energy Per Access: ${sramModelDataTable.get.sramA.referenceData.readEnergyPj} pJ")
      log(s"\t\t\tBank Write Energy Per Access: ${sramModelDataTable.get.sramA.referenceData.writeEnergyPj} pJ")
      log(s"\t\t\tBank Leakage Power: ${sramModelDataTable.get.sramA.referenceData.leakagePowerPw} pW")
      log(s"\t\t\tBank Area: ${sramModelDataTable.get.sramA.referenceData.areaUm2} um^2\n")
      log(s"\t\t\tTotal SRAM Capacity: ${sramModelDataTable.get.sramA.totalSramCapacityKb} KB")
      log(s"\t\t\tTotal SRAM Area: ${sramModelDataTable.get.sramA.totalSramSizeUm2} um^2")
      log(s"")
      log(s"\t\t\t[Weight SRAM B]")
      log(s"\t\t\tBank Count: ${sramModelDataTable.get.sramB.bankCount}")
      log(s"\t\t\tBank Capacity: ${sramModelDataTable.get.sramB.referenceData.capacityKb} KB")
      log(s"\t\t\tBank Bandwidth: ${sramModelDataTable.get.sramB.referenceData.bandwidthBits}")
      log(s"\t\t\tBank Read Energy Per Access: ${sramModelDataTable.get.sramB.referenceData.readEnergyPj} pJ")
      log(s"\t\t\tBank Write Energy Per Access: ${sramModelDataTable.get.sramB.referenceData.writeEnergyPj} pJ")
      log(s"\t\t\tBank Leakage Power: ${sramModelDataTable.get.sramB.referenceData.leakagePowerPw} pW")
      log(s"\t\t\tBank Area: ${sramModelDataTable.get.sramB.referenceData.areaUm2} um^2\n")
      log(s"\t\t\tTotal SRAM Capacity: ${sramModelDataTable.get.sramB.totalSramCapacityKb} KB")
      log(s"\t\t\tTotal SRAM Area: ${sramModelDataTable.get.sramB.totalSramSizeUm2} um^2")
      log(s"")
      log(s"\t\t\t[Output SRAM C]")
      log(s"\t\t\tBank Count: ${sramModelDataTable.get.sramC.bankCount}")
      log(s"\t\t\tBank Capacity: ${sramModelDataTable.get.sramC.referenceData.capacityKb} KB")
      log(s"\t\t\tBank Bandwidth: ${sramModelDataTable.get.sramC.referenceData.bandwidthBits}")
      log(s"\t\t\tBank Read Energy Per Access: ${sramModelDataTable.get.sramC.referenceData.readEnergyPj} pJ")
      log(s"\t\t\tBank Write Energy Per Access: ${sramModelDataTable.get.sramC.referenceData.writeEnergyPj} pJ")
      log(s"\t\t\tBank Leakage Power: ${sramModelDataTable.get.sramC.referenceData.leakagePowerPw} pW")
      log(s"\t\t\tBank Area: ${sramModelDataTable.get.sramC.referenceData.areaUm2} um^2\n")
      log(s"\t\t\tTotal SRAM Capacity: ${sramModelDataTable.get.sramC.totalSramCapacityKb} KB")
      log(s"\t\t\tTotal SRAM Area: ${sramModelDataTable.get.sramC.totalSramSizeUm2} um^2")
      log(s"")

      if(arraySynthesisSource.isEmpty){
        Console.err.println(s"[error] Array Synthesis Source is not defined when the results contain Energy")
        sys.exit(1)
      }

      if(arraySynthesisSource.get == ArraySynthesisSource.DesignCompiler){

        log(s"\t[Array Synthesis Results from Design Compiler]")
        log(s"\t\tArea: ${arraySynthesisData.get.areaUm2} um^2")
        log(s"\t\tSwitch Power: ${arraySynthesisData.get.switchPowerPw} pW")
        log(s"\t\tInternal Power: ${arraySynthesisData.get.internalPowerPw} pW")
        log(s"\t\tLeakage Power: ${arraySynthesisData.get.leakagePowerPw} pW")

      } else if(arraySynthesisSource.get == ArraySynthesisSource.FewShotPrediction) {
        log(s"\t[Array Synthesis Results from Few Shot Prediction]")
        log(s"\t\tArea: ${arraySynthesisData.get.areaUm2} um^2")
        log(s"\t\tSwitch Power: ${arraySynthesisData.get.switchPowerPw} pW")
        log(s"\t\tInternal Power: ${arraySynthesisData.get.internalPowerPw} pW")
        log(s"\t\tLeakage Power: ${arraySynthesisData.get.leakagePowerPw} pW")
      }

      log(s"")
    }

    if (isEnergyReportValid) {
      log("\t[Energy Analysis (pJ)]")

      log("\t\t[Off Chip Memory]")
      log(s"\t\tOff Chip Memory Read Energy: ${String.format("%.2f", offChipMemoryReadEnergyPj.get)} pJ")
      log(s"\t\tOff Chip Memory Write Energy: ${String.format("%.2f", offChipMemoryWriteEnergyPj.get)} pJ")
      log(s"\t\tOff Chip Memory Total Energy: ${String.format("%.2f", offChipMemoryEnergyPj.get)} pJ")
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
      log(s"\t\tEnergy: ${String.format("%.2f", arrayDynamicEnergyPj.get)} pJ")
      log(s"\t\tEnergy: ${String.format("%.2f", arrayLeakageEnergyPj.get)} pJ")
      log(s"\t\tEnergy: ${String.format("%.2f", arrayEnergy.get)} pJ")
      log("")

      log("\t\t[Total Energy]")
      log(s"\t\t${String.format("%.2f", energyPj.get)} pJ")
      log("")
//      log(s"\t\t[CSV Format (Off Chip Memory, SRAM A, SRAM B, SRAM C, ARRAY)] Energy")
//      log(s"\t\t${String.format("%.2f", offChipMemoryEnergyPj.get)}, " +
//        s"${String.format("%.2f", sramEnergyPjA.get)}, " +
//        s"${String.format("%.2f", sramEnergyPjB.get)}, " +
//        s"${String.format("%.2f", sramEnergyPjC.get)}, " +
//        s"${String.format("%.2f", arrayEnergy.get)}"
//      )
    }

    // Area Analysis
    if (isAreaReportValid) {
      log("\t[Area Analysis (um^2)]")
      log(s"\t\tSRAM A Area: ${String.format("%.2f", sramAreaUm2A.get)} um^2")
      log(s"\t\tSRAM B Area: ${String.format("%.2f", sramAreaUm2B.get)} um^2")
      log(s"\t\tSRAM C Area: ${String.format("%.2f", sramAreaUm2C.get)} um^2")
      log(s"\t\tArray Area: ${String.format("%.2f", arrayAreaUm2.get)} um^2")
      log(s"\t\tTotal Area: ${String.format("%.2f", areaUm2.get)} um^2")
      log(s"")
//      log(s"\t\t[CSV Format (SRAM A, SRAM B, SRAM C, ARRAY)] Area")
//      log(s"\t\t${String.format("%.2f", sramAreaUm2A.get)}, " +
//        s"${String.format("%.2f", sramAreaUm2B.get)}, " +
//        s"${String.format("%.2f", sramAreaUm2C.get)}, " +
//        s"${String.format("%.2f", arrayAreaUm2.get)}"
//      )
    }

    if (isEnergyReportValid && isAreaReportValid) {
      log("\t[Energy-Area-Delay Product]")
      log(s"\t\tEnergy-Area-Delay Product: ${String.format("%.2f", edap.get)} pJ*um^2")
      log(s"")
      log(s"\t[Final CSV Format (Array Active Cycle, Cycle, Area, Energy, Area Energy Delay Product, Average Memory Utilization)]")
      log(s"\t\t$arrayActiveCount, " +
        s"$cycle, " +
        s"${String.format("%.2f", areaUm2.get)}, " +
        s"${String.format("%.2f", energyPj.get)}, " +
        s"${String.format("%.2f", edap.get)}, " +
        s"${String.format("%.2f", averageMemoryUtilization)}")
      log(s"")
    }



  }

  private def isReferenceDataValid: Boolean = {
    offChipMemoryReferenceData.isDefined && sramModelDataTable.isDefined && offChipMemoryReferenceData.isDefined
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
      offChipMemoryReadEnergyPj.isDefined &&
      offChipMemoryWriteEnergyPj.isDefined &&
      offChipMemoryEnergyPj.isDefined &&
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

  def apply(wrongCycle: Long, wrongEnergy: Double, wrongArea: Double): SimulationResult = {
    val defaults = DefaultValues()

    SimulationResult(
      // 1. Initial Workload Metrics
      totalOperationNumber = defaults.intValue,
      tileSizeA = defaults.intValue,
      tileSizeB = defaults.intValue,
      tileSizeC = defaults.intValue,
      skipTileCountA = defaults.intValue,
      skipTileCountB = defaults.intValue,
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

      offChipMemoryReadAccessCount = defaults.longValue,
      offChipMemoryWriteAccessCount = defaults.longValue,
      sramReadAccessCountA = defaults.longValue,
      sramWriteAccessCountA = defaults.longValue,
      sramReadAccessCountB = defaults.longValue,
      sramWriteAccessCountB = defaults.longValue,

      offChipMemoryHitRatio = defaults.doubleValue,
      offChipMemoryMissRatio = defaults.doubleValue,

      sramHitRatioA = defaults.doubleValue,
      sramHitRatioB = defaults.doubleValue,
      sramMissRatioA = defaults.doubleValue,
      sramMissRatioB = defaults.doubleValue,
      sramHitRatio = defaults.doubleValue,
      sramMissRatio = defaults.doubleValue,

      // 4. Pipeline state
      offChipMemoryStallCount = defaults.intValue,

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

      averageMemoryUtilization = defaults.doubleValue,

      //6. Reference Data
      offChipMemoryReferenceData = None,
      sramModelDataTable = None,
      arraySynthesisSource = None,
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

      offChipMemoryReadEnergyPj = None,
      offChipMemoryWriteEnergyPj = None,
      offChipMemoryEnergyPj = None,

      arrayDynamicEnergyPj = None,
      arrayLeakageEnergyPj = None,
      arrayEnergy = None,

      energyPj = Some(wrongEnergy),

      //8. Area
      sramAreaUm2A = None,
      sramAreaUm2B = None,
      sramAreaUm2C = None,
      arrayAreaUm2 = None,
      areaUm2 = Some(wrongArea),

      //9. EDAP
      edap = None,
    )
  }
}
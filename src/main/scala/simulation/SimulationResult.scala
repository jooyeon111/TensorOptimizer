package simulation

import scala.collection.mutable


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


  dramLogs: mutable.Queue[DramLog],
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

//  arrayInputStallCount: Int,
//  arrayOutputStallCount: Int,

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
//
//  sramAreaMmA: Double,
//  sramAreaMmB: Double,
//  sramAreaMmC: Double,
//  dramAreaMm: Double,
//  arrayAreaMm: Double,
//  areaMm: Double,
//
//  sramReadEnergyPjA: Double,
//  sramWriteEnergyPjA: Double,
//  sramLeakageEnergyPjA: Double,
//  sramEnergyPjA: Double,
//
//  sramReadEnergyPjB: Double,
//  sramWriteEnergyPjB: Double,
//  sramLeakageEnergyPjB: Double,
//  sramEnergyPjB: Double,
//
//  sramReadEnergyPjC: Double,
//  sramWriteEnergyPjC: Double,
//  sramLeakageEnergyPjC: Double,
//  sramEnergyPjC: Double,
//
//  dramReadEnergyPj: Double,
//  dramWriteEnergyPj: Double,
//  dramEnergyPj: Double,
//
//  arrayDynamicEnergyPj: Double,
//  arrayLeakageEnergyPj: Double,
//  arrayEnergy: Double,
//
//  energyPj: Double

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
    log("\t\t[Bandwidth Information]")
    log(s"\t\tArray Input Bandwidth A: $arrayInputBandwidthA bit")
    log(s"\t\tArray Input Bandwidth B: $arrayInputBandwidthB bit")
    log(s"\t\tArray Output Bandwidth C: $arrayOutputBandwidthC bit")
    log(s"")
    log(s"\t\tArray Capacity A: $arrayCapacityA bit")
    log(s"\t\tArray Capacity B: $arrayCapacityB bit")
    log(s"")
    log("\t\t[Performance Metrics]")
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
    log("\t\t[Pipeline State]")
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
//    log(s"\t\tArray Input Stall Count: $arrayInputStallCount")
//    log(s"\t\tArray Output Stall Count: $arrayOutputStallCount")
    log(s"")
    log(s"\t\tSRAM C First Fill Up Cycle: $firstFillUpCycleC")
    log(s"\t\tSRAM C Buffer swap Success Count: $bufferSwapCountC")
    log(s"\t\tSRAM C Buffer Swap Stall Count: $bufferSwapStallCountC")
    log(s"")
    log("\t\t[Memory Utilization]")
    log(s"")
    log(s"\t\tSRAM A Average Memory Usage: ${String.format("%.2f", averageMemoryUsageKbA)} KB")
    log(s"\t\tSRAM A Average Memory Utilization: ${String.format("%.2f", averageMemoryUtilizationA)} %")
    log(s"\t\tSRAM B Average Memory Usage: ${String.format("%.2f", averageMemoryUsageKbB)} KB")
    log(s"\t\tSRAM B Average Memory Utilization: ${String.format("%.2f", averageMemoryUtilizationB)} %")
    log(s"\t\tSRAM C Average Memory Usage: ${String.format("%.2f", averageMemoryUsageKbC)} KB")
    log(s"\t\tSRAM C Average Memory Utilization: ${String.format("%.2f", averageMemoryUtilizationC)} %")
    log(s"")
//    log("\t\t[Area Analysis (mm^2)]")
//    log(s"")
//    log(s"\t\tSRAM A Area: ${String.format("%.2f", sramAreaMmA)} mm^2")
//    log(s"\t\tSRAM B Area: ${String.format("%.2f", sramAreaMmB)} mm^2")
//    log(s"\t\tSRAM C Area: ${String.format("%.2f", sramAreaMmC)} mm^2")
//    log(s"\t\tDRAM Area: ${String.format("%.2f", dramAreaMm)} mm^2")
//    log(s"\t\tArray Area: ${String.format("%.2f", arrayAreaMm)} mm^2")
//    log(s"\t\tTotal Area: ${String.format("%.2f", areaMm)} mm^2")
//    log(s"")
//    log("\t\t[Energy Analysis (pJ)]")
//    log(s"")
//    log(s"\t\tSRAM A Read Energy: ${String.format("%.2f", sramReadEnergyPjA)} pJ")
//    log(s"\t\tSRAM A Write Energy: ${String.format("%.2f", sramWriteEnergyPjA)} pJ")
//    log(s"\t\tSRAM A Leakage Energy: ${String.format("%.2f", sramLeakageEnergyPjA)} pJ")
//    log(s"\t\tSRAM A Total Energy: ${String.format("%.2f", sramEnergyPjA)} pJ")
//    log(s"")
//    log(s"\t\tSRAM B Read Energy: ${String.format("%.2f", sramReadEnergyPjB)} pJ")
//    log(s"\t\tSRAM B Write Energy: ${String.format("%.2f", sramWriteEnergyPjB)} pJ")
//    log(s"\t\tSRAM B Leakage Energy: ${String.format("%.2f", sramLeakageEnergyPjB)} pJ")
//    log(s"\t\tSRAM B Total Energy: ${String.format("%.2f", sramEnergyPjB)} pJ")
//    log(s"")
//    log(s"\t\tSRAM C Read Energy: ${String.format("%.2f", sramReadEnergyPjC)} pJ")
//    log(s"\t\tSRAM C Write Energy: ${String.format("%.2f", sramWriteEnergyPjC)} pJ")
//    log(s"\t\tSRAM C Leakage Energy: ${String.format("%.2f", sramLeakageEnergyPjC)} pJ")
//    log(s"\t\tSRAM C Total Energy: ${String.format("%.2f", sramEnergyPjC)} pJ")
//    log(s"")
//    log(s"\t\tDRAM Read Energy: ${String.format("%.2f", dramReadEnergyPj)} pJ")
//    log(s"\t\tDRAM Write Energy: ${String.format("%.2f", dramWriteEnergyPj)} pJ")
//    log(s"\t\tDRAM Total Energy: ${String.format("%.2f", dramEnergyPj)} pJ")
//    log(s"")
//    log(s"\t\tArray Dynamic Energy: ${String.format("%.2f", arrayDynamicEnergyPj)} pJ")
//    log(s"\t\tArray Leakage Energy: ${String.format("%.2f", arrayLeakageEnergyPj)} pJ")
//    log(s"\t\tArray Total Energy: ${String.format("%.2f", arrayEnergy)} pJ")
//    log(s"")
//    log(s"\t\tTotal Energy: ${String.format("%.2f", energyPj)} pJ")
//    log(s"")
//    log("\t\t[Compute-Memory Overlap Analysis]")
//    log("\t[DRAM Log]")
//    dramLogs.foreach { dramLog =>
//      log(s"\t\tCycle: ${dramLog.cycle}, DRAM Access State: ${dramLog.accessState.toString}")
//    }

  }

  def showSummary(loggerOption: LoggerOption): Unit = {
    setMode(loggerOption)
    log("[Summary Results]")
    log(s"\tCycle: $cycle")
//    log(s"\tArea: ${String.format("%.2f", areaMm)} mm^2")
//    log(s"\tEnergy: ${String.format("%.2f", energyPj)} pJ")
  }


}

object SimulationResult {
  def apply(wrongCycle: Long): SimulationResult = SimulationResult(
    totalOperationNumber = -1,
    tileSizeA = -1,
    tileSizeB = -1,
    tileSizeC = -1,
    trimTileCountA = -1,
    trimTileCountB = -1,
    singleBufferTileCapacityA = -1,
    singleBufferTileCapacityB = -1,
    singleBufferTileCapacityC = -1,

    arrayInputBandwidthA = -1,
    arrayInputBandwidthB = -1,
    arrayOutputBandwidthC = -1,
    arrayCapacityA = -1,
    arrayCapacityB = -1,

    cycle = wrongCycle,
    arrayActiveCount = -1,

    dramLogs = mutable.Queue.empty[DramLog],
    dramReadAccessCount = -1,
    dramWriteAccessCount = -1,
    sramReadAccessCountA = -1,
    sramWriteAccessCountA = -1,
    sramReadAccessCountB = -1,
    sramWriteAccessCountB = -1,

    dramHitRatio = -1,
    dramMissRatio = - 1,

    sramHitRatioA = -1,
    sramHitRatioB = -1,
    sramMissRatioA = -1,
    sramMissRatioB = -1,
    sramHitRatio = -1,
    sramMissRatio = -1,

    //Buffer state
    dramStallCount = -1,

    firstFillUpCycleA = -1,
    bufferSwapCountA = -1,
    bufferSwapStallCountA = -1,

    firstFillUpCycleB = -1,
    bufferSwapCountB = -1,
    bufferSwapStallCountB = -1,

//    arrayInputStallCount = -1,
//    arrayOutputStallCount = -1,

    firstFillUpCycleC = -1,
    bufferSwapCountC = -1,
    bufferSwapStallCountC = -1,

    averageMemoryUsageKbA = -1,
    averageMemoryUtilizationA = -1,

    averageMemoryUsageKbB = -1,
    averageMemoryUtilizationB = -1,

    averageMemoryUsageKbC = -1,
    averageMemoryUtilizationC = -1,
//
//    sramAreaMmA = -1,
//    sramAreaMmB = -1,
//    sramAreaMmC = -1,
//    dramAreaMm = -1,
//    arrayAreaMm = -1,
//    areaMm = -1,
//
//    sramReadEnergyPjA = -1,
//    sramWriteEnergyPjA = -1,
//    sramLeakageEnergyPjA = -1,
//    sramEnergyPjA = -1,
//
//    sramReadEnergyPjB = -1,
//    sramWriteEnergyPjB = -1,
//    sramLeakageEnergyPjB = -1,
//    sramEnergyPjB = -1,
//
//    sramReadEnergyPjC = -1,
//    sramWriteEnergyPjC = -1,
//    sramLeakageEnergyPjC = -1,
//    sramEnergyPjC = -1,
//
//    dramReadEnergyPj = -1,
//    dramWriteEnergyPj = -1,
//    dramEnergyPj = -1,
//
//    arrayDynamicEnergyPj = - 1,
//    arrayLeakageEnergyPj = - 1,
//    arrayEnergy = - 1,
//
//    energyPj = -1,
  )
}

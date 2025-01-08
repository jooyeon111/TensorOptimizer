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

  //2. Performance Metrics
  cycle: Long,
  arrayActiveCount: Int,
  arrayHoldUpCount: Int,
  dramHolUpCount: Int,

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

  //3. Memory Utilization
  sramBufferToggleCountA: Int,
  sramBufferToggleCountB: Int,
  sramBufferToggleCountC: Int,

  averageMemoryUsageKbA: Double,
  averageMemoryUtilizationA: Double,

  averageMemoryUsageKbB: Double,
  averageMemoryUtilizationB: Double,

  averageMemoryUsageKbC: Double,
  averageMemoryUtilizationC: Double,

  //4. Area Analysis
  sramAreaMmA: Double,
  sramAreaMmB: Double,
  sramAreaMmC: Double,
  dramAreaMm: Double,
  arrayAreaMm: Double,
  areaMm: Double,

  //5. Energy Analysis
  sramReadEnergyPjA: Double,
  sramWriteEnergyPjA: Double,
  sramLeakageEnergyPjA: Double,
  sramEnergyPjA: Double,

  sramReadEnergyPjB: Double,
  sramWriteEnergyPjB: Double,
  sramLeakageEnergyPjB: Double,
  sramEnergyPjB: Double,

  sramReadEnergyPjC: Double,
  sramWriteEnergyPjC: Double,
  sramLeakageEnergyPjC: Double,
  sramEnergyPjC: Double,

  dramReadEnergyPj: Double,
  dramWriteEnergyPj: Double,
  dramLeakageEnergyPj: Double,
  dramEnergyPj: Double,

  arrayDynamicEnergyPj: Double,
  arrayLeakageEnergyPj: Double,
  arrayEnergy: Double,

  energyPj: Double

) extends Logger {

  def printFullResults(loggerOption: LoggerOption): Unit = {
    setMode(loggerOption)
    log(s"[Simulation Results]")
    log("[Workload Metrics]")
    log(s"")
    log(s"\tTotal Operation Number: $totalOperationNumber")
    log(s"\tTile Size A: $tileSizeA")
    log(s"\tTile Size B: $tileSizeB")
    log(s"\tTile Size C: $tileSizeC")
    log(s"\tTile A Trim Count: $trimTileCountA")
    log(s"\tTile B Trim Count: $trimTileCountB")
    log(s"")
    log("[Performance Metrics]")
    log(s"")
    log(s"\tTotal Cycle: $cycle")
    log(s"")
    log(s"\tArray Active Count: $arrayActiveCount")
    log(s"\tArray Stall Cycle: $arrayHoldUpCount")
    log(s"\tDram Hold Cycle: $dramHolUpCount")
    log(s"")
    log(s"\tDRAM Read Access Count: $dramReadAccessCount")
    log(s"\tDRAM Write Access Count: $dramWriteAccessCount")
    log(s"\tSRAM A Read Access Count: $sramReadAccessCountA")
    log(s"\tSRAM A Write Access Count: $sramWriteAccessCountA")
    log(s"\tSRAM B Read Access Count: $sramReadAccessCountB")
    log(s"\tSRAM B Write Access Count: $sramWriteAccessCountB")
    log(s"")
    log(s"\tDRAM Hit Ratio: ${String.format("%.2f", dramHitRatio)} %")
    log(s"\tDRAM Miss Ratio: ${String.format("%.2f", dramMissRatio)} %")
    log(s"")
    log(s"\tSRAM A Hit Ratio: ${String.format("%.2f", sramHitRatioA)} %")
    log(s"\tSRAM B Hit Ratio: ${String.format("%.2f", sramHitRatioB)} %")
    log(s"\tSRAM A Miss Ratio: ${String.format("%.2f", sramMissRatioA)} %")
    log(s"\tSRAM B Miss Ratio: ${String.format("%.2f", sramMissRatioB)} %")
    log(s"\tSRAM Hit Ratio: ${String.format("%.2f", sramHitRatio)} %")
    log(s"\tSRAM Miss Ratio: ${String.format("%.2f", sramMissRatio)} %")
    log(s"")
    log("[Memory Utilization]")
    log(s"")
    log(s"\tSRAM A Buffer Toggle Count: $sramBufferToggleCountA")
    log(s"\tSRAM B Buffer Toggle Count: $sramBufferToggleCountB")
    log(s"\tSRAM C Buffer Toggle Count: $sramBufferToggleCountC")
    log(s"")
    log(s"\tSRAM A Average Memory Usage: ${String.format("%.2f", averageMemoryUsageKbA)} KB")
    log(s"\tSRAM A Average Memory Utilization: ${String.format("%.2f", averageMemoryUtilizationA)} %")
    log(s"\tSRAM B Average Memory Usage: ${String.format("%.2f", averageMemoryUsageKbB)} KB")
    log(s"\tSRAM B Average Memory Utilization: ${String.format("%.2f", averageMemoryUtilizationB)} %")
    log(s"\tSRAM C Average Memory Usage: ${String.format("%.2f", averageMemoryUsageKbC)} KB")
    log(s"\tSRAM C Average Memory Utilization: ${String.format("%.2f", averageMemoryUtilizationC)} %")
    log(s"")
    log("[Area Analysis (mm^2)]")
    log(s"")
    log(s"\tSRAM A Area: ${String.format("%.2f", sramAreaMmA)} mm^2")
    log(s"\tSRAM B Area: ${String.format("%.2f", sramAreaMmB)} mm^2")
    log(s"\tSRAM C Area: ${String.format("%.2f", sramAreaMmC)} mm^2")
    log(s"\tDRAM Area: ${String.format("%.2f", dramAreaMm)} mm^2")
    log(s"\tArray Area: ${String.format("%.2f", arrayAreaMm)} mm^2")
    log(s"\tTotal Area: ${String.format("%.2f", areaMm)} mm^2")
    log(s"")
    log("[Energy Analysis (pJ)]")
    log(s"")
    log(s"\tSRAM A Read Energy: ${String.format("%.2f", sramReadEnergyPjA)} pJ")
    log(s"\tSRAM A Write Energy: ${String.format("%.2f", sramWriteEnergyPjA)} pJ")
    log(s"\tSRAM A Leakage Energy: ${String.format("%.2f", sramLeakageEnergyPjA)} pJ")
    log(s"\tSRAM A Total Energy: ${String.format("%.2f", sramEnergyPjA)} pJ")
    log(s"")
    log(s"\tSRAM B Read Energy: ${String.format("%.2f", sramReadEnergyPjB)} pJ")
    log(s"\tSRAM B Write Energy: ${String.format("%.2f", sramWriteEnergyPjB)} pJ")
    log(s"\tSRAM B Leakage Energy: ${String.format("%.2f", sramLeakageEnergyPjB)} pJ")
    log(s"\tSRAM B Total Energy: ${String.format("%.2f", sramEnergyPjB)} pJ")
    log(s"")
    log(s"\tSRAM C Read Energy: ${String.format("%.2f", sramReadEnergyPjC)} pJ")
    log(s"\tSRAM C Write Energy: ${String.format("%.2f", sramWriteEnergyPjC)} pJ")
    log(s"\tSRAM C Leakage Energy: ${String.format("%.2f", sramLeakageEnergyPjC)} pJ")
    log(s"\tSRAM C Total Energy: ${String.format("%.2f", sramEnergyPjC)} pJ")
    log(s"")
    log(s"\tDRAM Read Energy: ${String.format("%.2f", dramReadEnergyPj)} pJ")
    log(s"\tDRAM Write Energy: ${String.format("%.2f", dramWriteEnergyPj)} pJ")
    log(s"\tDRAM Leakage Energy: ${String.format("%.2f", dramLeakageEnergyPj)} pJ")
    log(s"\tDRAM Total Energy: ${String.format("%.2f", dramEnergyPj)} pJ")
    log(s"")
    log(s"\tArray Dynamic Energy: ${String.format("%.2f", arrayDynamicEnergyPj)} pJ")
    log(s"\tArray Leakage Energy: ${String.format("%.2f", arrayLeakageEnergyPj)} pJ")
    log(s"\tArray Total Energy: ${String.format("%.2f", arrayEnergy)} pJ")
    log(s"")
    log(s"\tTotal Energy: ${String.format("%.2f", energyPj)} pJ")
  }

  def showSummary(loggerOption: LoggerOption): Unit = {
    setMode(loggerOption)
    log("[Summary Results]")
    log(s"\tCycle: $cycle")
    log(s"\tArea: ${String.format("%.2f", areaMm)} mm^2")
    log(s"\tEnergy: ${String.format("%.2f", energyPj)} pJ")
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
    cycle = wrongCycle,
    arrayActiveCount = -1,
    arrayHoldUpCount = -1,
    dramHolUpCount = -1,
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
    sramBufferToggleCountA = -1,
    sramBufferToggleCountB = -1,
    sramBufferToggleCountC = -1,
    averageMemoryUsageKbA = -1,
    averageMemoryUtilizationA = -1,
    averageMemoryUsageKbB = -1,
    averageMemoryUtilizationB = -1,
    averageMemoryUsageKbC = -1,
    averageMemoryUtilizationC = -1,
    sramAreaMmA = -1,
    sramAreaMmB = -1,
    sramAreaMmC = -1,
    dramAreaMm = -1,
    arrayAreaMm = -1,
    areaMm = -1,
    sramReadEnergyPjA = -1,
    sramWriteEnergyPjA = -1,
    sramLeakageEnergyPjA = -1,
    sramEnergyPjA = -1,
    sramReadEnergyPjB = -1,
    sramWriteEnergyPjB = -1,
    sramLeakageEnergyPjB = -1,
    sramEnergyPjB = -1,
    sramReadEnergyPjC = -1,
    sramWriteEnergyPjC = -1,
    sramLeakageEnergyPjC = -1,
    sramEnergyPjC = -1,
    dramReadEnergyPj = -1,
    dramWriteEnergyPj = -1,
    dramLeakageEnergyPj = -1,
    dramEnergyPj = -1,
    arrayDynamicEnergyPj = - 1,
    arrayLeakageEnergyPj = - 1,
    arrayEnergy = - 1,
    energyPj = -1,
  )
}

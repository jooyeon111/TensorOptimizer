package simulation

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import common.Dataflow
import scala.collection.parallel.CollectionConverters._

class ArchitectureEvaluator(
  val simConfig: SystemArchitectureOptimizer.SimulationConfig,
  val architectureCandidates: ArrayBuffer[Architecture],
  val minSramSize: Int,
  val loggerOption: LoggerOption,
) extends Logger {

  setMode(loggerOption)

  private val executableCandidates = ArrayBuffer.empty[Architecture]
  private val calculatedAllResults = ArrayBuffer.empty[ArchitectureResult]
  private val rankedCalculatedResults = ArrayBuffer.empty[ArchitectureResult]
  private val archOptimizedResults = ArrayBuffer.empty[ArchitectureResult]
  private val singleSramOptimizedResults = ArrayBuffer.empty[ArchitectureResult]
  private val rankedSingleSramOptimizedResults = ArrayBuffer.empty[ArchitectureResult]

  private val processOneMargin: Double = 30.0
  private val processTwoMargin: Double = 100.0
  private val processThreeMargin: Double = 5.0

  def run(): Unit = {

    log("[Optimization Process]")
    executableCandidates ++= validateCandidates(architectureCandidates)
    logExecutableArch(
      candidates = architectureCandidates,
      validCandidates = executableCandidates,
    )

    executableCandidates.foreach(logSummary)

    //simulate all configs
    calculatedAllResults ++= process1(executableCandidates)
    rankedCalculatedResults ++= rankResults(calculatedAllResults, processOneMargin)
    logProcessOne(
      validCandidates = executableCandidates,
      simulatedCandidates = calculatedAllResults,
      rankedSimulatedCandidates = rankedCalculatedResults,
    )

//    rankedCalculatedResults.foreach(logSummary)

    //optimize arch
    archOptimizedResults ++= process2(rankedCalculatedResults, processTwoMargin)

    //optimize single sram
    singleSramOptimizedResults ++= process3(archOptimizedResults)
    rankedSingleSramOptimizedResults ++= rankResults(singleSramOptimizedResults, processThreeMargin)
    logProcessThree(
      preProcess = archOptimizedResults,
      postProcess = singleSramOptimizedResults,
      rankedPostProcess = rankedSingleSramOptimizedResults
    )

    logOptimizationSummary()

  }

  def logTopResults(): Unit = {
    log(s"[Show Top Results]")
    rankedSingleSramOptimizedResults.foreach(logSummary)
    log("")
  }

  def logTopResultsCsv(): Unit = {
    log(s"[Show Top Results CSV Format]")
    rankedSingleSramOptimizedResults.foreach(logCsv)
    log("")
  }

  private def validateCandidates(archBuffer: ArrayBuffer[Architecture]): ArrayBuffer[Architecture] = {
    log("\t[Check Initial Streaming Dimension]")
    println("Check Initial Streaming Dimension")
    checkInitialStreamingDimensions(archBuffer)
  }

  private def process1(archBuffer: ArrayBuffer[Architecture]): ArrayBuffer[ArchitectureResult] = {
    log("\t[Process1: Find High Efficiency STA configs]")
    println("Process1: Find High Efficiency STA configs")
    evaluateAllArchitectures(archBuffer)
  }

  private def process2(archResultBuffer: ArrayBuffer[ArchitectureResult], processMargin: Double): ArrayBuffer[ArchitectureResult] = {
    log(s"\t[Process2: Optimize Architecture]")
    println("Process2: Optimize Architecture")
    optimizeSramStreamingTradeOffs(archResultBuffer, processMargin)
  }

  private def process3(archResultBuffer: ArrayBuffer[ArchitectureResult]): ArrayBuffer[ArchitectureResult] = {
    log(s"\t[Process3: Optimize Single SRAM]")
    println("Process3: Optimize Single SRAM")
    halfSingleSramSizes(archResultBuffer)
  }

  private def logExecutableArch(
    candidates: ArrayBuffer[Architecture],
    validCandidates: ArrayBuffer[Architecture]
  ): Unit = {
    log(s"\t\t${candidates.size} architectures were candidates")
    log(s"\t\t${validCandidates.size} architectures were valid")
    log("")
  }

  private def logProcessOne(
    validCandidates: ArrayBuffer[Architecture],
    simulatedCandidates: ArrayBuffer[ArchitectureResult],
    rankedSimulatedCandidates: ArrayBuffer[ArchitectureResult],
  ): Unit = {
    log(s"\t\t${validCandidates.size} valid architectures were passed to process")
    log(s"\t\t${simulatedCandidates.size} architectures were successfully processed")
    log(s"\t\t${rankedSimulatedCandidates.size} architectures meet performance criteria")
//    logStatistics(rankedSimulatedCandidates)
    log("")
  }

  private def logProcessTwo(
    preProcess: ArrayBuffer[ArchitectureResult],
    postProcess: ArrayBuffer[ArchitectureResult],
    rankedPostProcess: ArrayBuffer[ArchitectureResult],
    iteration: Int,
  ): Unit = {

    log(s"\t\t[Iteration $iteration Result]")
    log(s"\t\t\t${preProcess.size} architectures were processed")
    log(s"\t\t\t${postProcess.size} optimized architectures were evaluated")
    log(s"\t\t\t${rankedPostProcess.size} architectures meet performance criteria")

//    log(s"\t\tPre Process Results")
//    preProcess.foreach(logSummary)
//    postProcess.foreach(logSummary)

    // Calculate improvement statistics
    if (preProcess.nonEmpty && rankedPostProcess.nonEmpty) {
      val preProcessBest = preProcess.head
      val postProcessBest = rankedPostProcess.head

      // Cycle improvement
      val cycleImprovement = (preProcessBest.simulationResult.cycle - postProcessBest.simulationResult.cycle).toDouble /
        preProcessBest.simulationResult.cycle * 100
      log(s"\t\t\tCycle time: ${String.format("%.2f", cycleImprovement)}% improvement")

      // Resource usage changes
      val avgSramSizeBefore = (preProcessBest.architecture.singleBufferLimitKbA +
        preProcessBest.architecture.singleBufferLimitKbB +
        preProcessBest.architecture.singleBufferLimitKbC) / 3.0

      val avgSramSizeAfter = (postProcessBest.architecture.singleBufferLimitKbA +
        postProcessBest.architecture.singleBufferLimitKbB +
        postProcessBest.architecture.singleBufferLimitKbC) / 3.0

      val sramReduction = (avgSramSizeBefore - avgSramSizeAfter) / avgSramSizeBefore * 100
      log(s"\t\t\tAverage SRAM size: ${String.format("%.2f", sramReduction)}% reduction")

      if (preProcessBest.architecture.streamingDimensionSize != postProcessBest.architecture.streamingDimensionSize) {
        val streamDimChange = (preProcessBest.architecture.streamingDimensionSize - postProcessBest.architecture.streamingDimensionSize).toDouble /
          preProcessBest.architecture.streamingDimensionSize * 100
        log(s"\t\t\tStreaming dimension: ${String.format("%.2f", streamDimChange)}% reduction")
      }

      val initialAvgUtilization = preProcessBest.simulationResult.averageMemoryUtilization
      val finalAvgUtilization = postProcessBest.simulationResult.averageMemoryUtilization

      val utilizationImprovement = finalAvgUtilization - initialAvgUtilization
      log(s"\t\t\tAverage memory utilization ${String.format("%.2f", utilizationImprovement)}% increase")

      // Energy improvement if available
      if (preProcessBest.simulationResult.energyPj.isDefined && postProcessBest.simulationResult.energyPj.isDefined) {
        val energyImprovement = (preProcessBest.simulationResult.energyPj.get - postProcessBest.simulationResult.energyPj.get) /
          preProcessBest.simulationResult.energyPj.get * 100
        log(s"\t\t\tEnergy: ${String.format("%.2f", energyImprovement)}% improvement")
      }

      // Area improvement if available
      if (preProcessBest.simulationResult.areaUm2.isDefined && postProcessBest.simulationResult.areaUm2.isDefined) {
        val areaImprovement = (preProcessBest.simulationResult.areaUm2.get - postProcessBest.simulationResult.areaUm2.get) /
          preProcessBest.simulationResult.areaUm2.get * 100
        log(s"\t\t\tArea: ${String.format("%.2f", areaImprovement)}% improvement")
      }

      if (preProcessBest.simulationResult.edap.isDefined && postProcessBest.simulationResult.edap.isDefined) {
        val energyAreaProductImprovement= (preProcessBest.simulationResult.edap.get - postProcessBest.simulationResult.edap.get) /
          preProcessBest.simulationResult.edap.get * 100
        log(s"\t\t\tArea-Energy-Delay Product: ${String.format("%.6f", energyAreaProductImprovement)}% improvement")
      }

    }

    log("")

  }

  private def logProcessThree(
    preProcess: ArrayBuffer[ArchitectureResult],
    postProcess: ArrayBuffer[ArchitectureResult],
    rankedPostProcess: ArrayBuffer[ArchitectureResult],
  ): Unit = {

    log(s"\t\t${preProcess.size} architectures were processed")
    log(s"\t\t${postProcess.size} optimized architectures were evaluated")
    log(s"\t\t${rankedPostProcess.size} architectures meet performance criteria")


    // Calculate improvement statistics
    val preProcessBest = preProcess.head
    val postProcessBest = rankedPostProcess.head

    // Cycle improvement
    val cycleImprovement = (preProcessBest.simulationResult.cycle - postProcessBest.simulationResult.cycle).toDouble /
      preProcessBest.simulationResult.cycle * 100
    log(s"\t\tCycle time: ${String.format("%.2f", cycleImprovement)}% improvement")
    val avgSramSizeBefore = (preProcessBest.architecture.singleBufferLimitKbA +
      preProcessBest.architecture.singleBufferLimitKbB +
      preProcessBest.architecture.singleBufferLimitKbC) / 3.0

    val avgSramSizeAfter = (postProcessBest.architecture.singleBufferLimitKbA +
      postProcessBest.architecture.singleBufferLimitKbB +
      postProcessBest.architecture.singleBufferLimitKbC) / 3.0

    val sramReduction = (avgSramSizeBefore - avgSramSizeAfter) / avgSramSizeBefore * 100
    log(s"\t\tAverage SRAM size: ${String.format("%.2f", sramReduction)}% reduction")

    if (preProcessBest.architecture.streamingDimensionSize != postProcessBest.architecture.streamingDimensionSize) {
      val streamDimChange = (preProcessBest.architecture.streamingDimensionSize - postProcessBest.architecture.streamingDimensionSize).toDouble /
        preProcessBest.architecture.streamingDimensionSize * 100
      log(s"\t\tStreaming dimension: ${String.format("%.2f", streamDimChange)}% reduction")
    }

    val initialAvgUtilization = (preProcessBest.simulationResult.averageMemoryUtilizationA
      + preProcessBest.simulationResult.averageMemoryUtilizationB
      + preProcessBest.simulationResult.averageMemoryUtilizationC) / 3.0

    val finalAvgUtilization = (postProcessBest.simulationResult.averageMemoryUtilizationA
      + postProcessBest.simulationResult.averageMemoryUtilizationB
      + postProcessBest.simulationResult.averageMemoryUtilizationC) / 3.0

    val utilizationImprovement = finalAvgUtilization - initialAvgUtilization
    log(s"\t\tAverage memory utilization ${String.format("%.2f", utilizationImprovement)}% increase")

    // Energy improvement if available
    if (preProcessBest.simulationResult.energyPj.isDefined && postProcessBest.simulationResult.energyPj.isDefined) {
      val energyImprovement = (preProcessBest.simulationResult.energyPj.get - postProcessBest.simulationResult.energyPj.get) /
        preProcessBest.simulationResult.energyPj.get * 100
      log(s"\t\tEnergy: ${String.format("%.2f", energyImprovement)}% improvement")
    }

    // Area improvement if available
    if (preProcessBest.simulationResult.areaUm2.isDefined && postProcessBest.simulationResult.areaUm2.isDefined) {
      val areaImprovement = (preProcessBest.simulationResult.areaUm2.get - postProcessBest.simulationResult.areaUm2.get) /
        preProcessBest.simulationResult.areaUm2.get * 100
      log(s"\t\tArea: ${String.format("%.2f", areaImprovement)}% improvement")
    }

    if (preProcessBest.simulationResult.edap.isDefined && postProcessBest.simulationResult.edap.isDefined) {
      val energyAreaProductImprovement= (preProcessBest.simulationResult.edap.get - postProcessBest.simulationResult.edap.get) /
        preProcessBest.simulationResult.edap.get * 100
      log(s"\t\tEnergy Area Delay Product: ${String.format("%.6f", energyAreaProductImprovement)}% improvement")
    }

//    logStatistics(rankedPostProcess)

  }

//  private def logStatistics(resultBuffer: ArrayBuffer[ArchitectureResult]): Unit = {
//    val cycleTimes = resultBuffer.map(_.simulationResult.cycle)
//    val minCycle = cycleTimes.min
//    val maxCycle = cycleTimes.max
//    val avgCycle = cycleTimes.sum / cycleTimes.length
//
//    log("")
//    log("\t\t[Performance Statistics]")
//    log(s"\t\t\tMinimum cycle time: $minCycle")
//    log(s"\t\t\tMaximum cycle time: $maxCycle")
//    log(s"\t\t\tAverage cycle time: $avgCycle")
//
//    // Show energy and area metrics if available
//    if (resultBuffer.exists(_.simulationResult.energyPj.isDefined)) {
//      val energyValues = resultBuffer.flatMap(_.simulationResult.energyPj)
//      if (energyValues.nonEmpty) {
//
//        val minEnergy = energyValues.min
//        val maxEnergy = energyValues.max
//        val avgEnergy = energyValues.sum / energyValues.length
//
//        log(s"\t\t\tMinimum energy: ${String.format("%.2f", minEnergy)} pJ")
//        log(s"\t\t\tMaximum energy: ${String.format("%.2f", maxEnergy)} pJ")
//        log(s"\t\t\tAverage energy: ${String.format("%.2f", avgEnergy)} pJ")
//      }
//    }
//
//    if (resultBuffer.exists(_.simulationResult.areaUm2.isDefined)) {
//      val areaValues = resultBuffer.flatMap(_.simulationResult.areaUm2)
//      if (areaValues.nonEmpty) {
//        val minArea = areaValues.min
//        val maxArea = areaValues.max
//        val avgArea = areaValues.sum / areaValues.length
//
//        log(s"\t\t\tMinimum area: ${String.format("%.2f", minArea)} um²")
//        log(s"\t\t\tMaximum area: ${String.format("%.2f", maxArea)} um²")
//        log(s"\t\t\tAverage area: ${String.format("%.2f", avgArea)} um²")
//      }
//    }
//
//    if (resultBuffer.exists(_.simulationResult.edap.isDefined)) {
//      val energyAreaDelayProductValues = resultBuffer.flatMap(_.simulationResult.edap)
//      if (energyAreaDelayProductValues.nonEmpty) {
//        val minArea = energyAreaDelayProductValues.min
//        val maxArea = energyAreaDelayProductValues.max
//        val avgArea = energyAreaDelayProductValues.sum / energyAreaDelayProductValues.length
//
//        log(s"\t\t\tMinimum area and energy product: ${String.format("%.2f", minArea)} pJ*um^2")
//        log(s"\t\t\tMaximum area and energy product: ${String.format("%.2f", maxArea)} pJ*um^2")
//        log(s"\t\t\tAverage area and energy product: ${String.format("%.2f", avgArea)} pJ*um^2")
//      }
//    }
//
//
//  }


  private def logSummary(ArchitectureResult: ArchitectureResult): Unit = {
    val simulationResult = ArchitectureResult.simulationResult
    val architecture = ArchitectureResult.architecture

    if(simulationResult.isEnergyReportValid && simulationResult.isAreaReportValid){
      log(s"\t[${architecture.arrayConfig.arrayConfigString}]")
      log(s"\t\tCycle: ${simulationResult.cycle}")
      log(s"\t\tArea: ${String.format("%.2f", simulationResult.areaUm2.get)} um²")
      log(s"\t\tEnergy: ${String.format("%.2f", simulationResult.energyPj.get)} pJ")
      log(s"\t\tEnergy Delay Area Product: ${String.format("%.2f", simulationResult.edap.get)} pJ*um^2*cycle")
      log(s"\t\tStreaming Dimension Size: ${architecture.streamingDimensionSize}")
      log(s"\t\tSingleBuffer A: ${architecture.singleBufferLimitKbA} KB")
      log(s"\t\tSingleBuffer B: ${architecture.singleBufferLimitKbB} KB")
      log(s"\t\tSingleBuffer C: ${architecture.singleBufferLimitKbC} KB")
      log(s"\t\tAverage Memory Utilization: ${String.format("%.2f", simulationResult.averageMemoryUtilization)} %\n")
      log(s"\t\t[Array Bandwidth Information]")
      log(s"\t\t\tInput Bandwidth A: ${architecture.arrayConfig.bandwidthOfInputA}")
      log(s"\t\t\tInput Bandwidth B: ${architecture.arrayConfig.bandwidthOfInputB}")
      log(s"\t\t\tOutput Bandwidth C: ${architecture.arrayConfig.outputBandwidth}\n")
      log(s"\t\t[SRAM Modeling Information]")
      log(s"\t\t\t[Input SRAM A]")
      log(s"\t\t\t\tBank Count: ${simulationResult.sramModelDataTable.get.sramA.bankCount}")
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramA.referenceData.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramA.referenceData.bandwidthBits}")

      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramA.referenceData.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramA.referenceData.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramA.referenceData.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramA.referenceData.areaUm2} um^2\n")
      log(s"\t\t\t\tTotal SRAM Capacity: ${simulationResult.sramModelDataTable.get.sramA.totalSramCapacityKb} KB")
      log(s"\t\t\t\tTotal SRAM Area: ${simulationResult.sramModelDataTable.get.sramA.totalSramSizeUm2} um^2")
      log(s"")
      log(s"\t\t\t[Weight SRAM B]")
      log(s"\t\t\t\tBank Count: ${simulationResult.sramModelDataTable.get.sramB.bankCount}")
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramB.referenceData.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramB.referenceData.bandwidthBits}")
      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramB.referenceData.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramB.referenceData.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramB.referenceData.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramB.referenceData.areaUm2} um^2\n")
      log(s"\t\t\t\tTotal SRAM Capacity: ${simulationResult.sramModelDataTable.get.sramB.totalSramCapacityKb} KB")
      log(s"\t\t\t\tTotal SRAM Area: ${simulationResult.sramModelDataTable.get.sramB.totalSramSizeUm2} um^2")
      log(s"")
      log(s"\t\t\t[Output SRAM C]")
      log(s"\t\t\t\tBank Count: ${simulationResult.sramModelDataTable.get.sramC.bankCount}")
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramC.referenceData.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramC.referenceData.bandwidthBits}")
      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramC.referenceData.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramC.referenceData.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramC.referenceData.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramC.referenceData.areaUm2} um^2\n")
      log(s"\t\t\t\tTotal SRAM Capacity: ${simulationResult.sramModelDataTable.get.sramC.totalSramCapacityKb} KB")
      log(s"\t\t\t\tTotal SRAM Area: ${simulationResult.sramModelDataTable.get.sramC.totalSramSizeUm2} um^2\n")

    } else {
      log(s"\t[${architecture.arrayConfig.arrayConfigString}]")
      log(s"\t\tCycle: ${simulationResult.cycle},")
      log(s"\t\tStreaming Dimension Size: ${architecture.streamingDimensionSize}")
      log(s"\t\tSingleBuffer A: ${architecture.singleBufferLimitKbA} KB")
      log(s"\t\tSingleBuffer B: ${architecture.singleBufferLimitKbB} KB")
      log(s"\t\tSingleBuffer C: ${architecture.singleBufferLimitKbC} KB")
      log(s"\t\tAverage Memory Utilization: ${String.format("%.2f", simulationResult.averageMemoryUtilization)} %\n")
      log(s"\t\t[Array Bandwidth Information]")
      log(s"\t\t\tInput Bandwidth A: ${architecture.arrayConfig.bandwidthOfInputA}")
      log(s"\t\t\tInput Bandwidth B: ${architecture.arrayConfig.bandwidthOfInputB}")
      log(s"\t\t\tOutput Bandwidth C: ${architecture.arrayConfig.outputBandwidth}\n")
      log(s"\t\t[SRAM Modeling information]")
      log(s"\t\t\t[Input SRAM A]")
      log(s"\t\t\t\tBank Count: ${simulationResult.sramModelDataTable.get.sramA.bankCount}")
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramA.referenceData.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramA.referenceData.bandwidthBits}")
      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramA.referenceData.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramA.referenceData.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramA.referenceData.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramA.referenceData.areaUm2} um^2\n")
      log(s"\t\t\t\tTotal SRAM Capacity: ${simulationResult.sramModelDataTable.get.sramA.totalSramCapacityKb} KB")
      log(s"\t\t\t\tTotal SRAM Area: ${simulationResult.sramModelDataTable.get.sramA.totalSramSizeUm2} um^2")
      log(s"")
      log(s"\t\t\t[Weight SRAM B]")
      log(s"\t\t\t\tBank Count: ${simulationResult.sramModelDataTable.get.sramB.bankCount}")
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramB.referenceData.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramB.referenceData.bandwidthBits}")
      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramB.referenceData.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramB.referenceData.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramB.referenceData.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramB.referenceData.areaUm2} um^2\n")
      log(s"\t\t\t\tTotal SRAM Capacity: ${simulationResult.sramModelDataTable.get.sramB.totalSramCapacityKb} KB")
      log(s"\t\t\t\tTotal SRAM Area: ${simulationResult.sramModelDataTable.get.sramB.totalSramSizeUm2} um^2")
      log(s"")
      log(s"\t\t\t[Output SRAM C]")
      log(s"\t\t\t\tBank Count: ${simulationResult.sramModelDataTable.get.sramC.bankCount}")
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramC.referenceData.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramC.referenceData.bandwidthBits}")
      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramC.referenceData.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramC.referenceData.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramC.referenceData.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramC.referenceData.areaUm2} um^2\n")
      log(s"\t\t\t\tTotal SRAM Capacity: ${simulationResult.sramModelDataTable.get.sramC.totalSramCapacityKb} KB")
      log(s"\t\t\t\tTotal SRAM Area: ${simulationResult.sramModelDataTable.get.sramC.totalSramSizeUm2} um^2\n")

    }

  }

  private def logCsv(ArchitectureResult: ArchitectureResult): Unit = {

    val simulationResult = ArchitectureResult.simulationResult
    val architecture = ArchitectureResult.architecture

    if(simulationResult.isEnergyReportValid && simulationResult.isAreaReportValid) {
      log(s"\t${architecture.arrayConfig.dataflow}, " +
        s"${architecture.arrayConfig.asArrayDimension.arrayDimensionString}, " +
        s"${simulationResult.cycle}, " +
        s"${String.format("%.2f", simulationResult.areaUm2.get)}, " +
        s"${String.format("%.2f", simulationResult.energyPj.get)}, " +
        s"${String.format("%.6f", simulationResult.edap.get)}, " +
        s"${architecture.streamingDimensionSize}, " +
        s"${architecture.singleBufferLimitKbA}, " +
        s"${architecture.singleBufferLimitKbB}, " +
        s"${architecture.singleBufferLimitKbC}, " +
        s"${String.format("%.2f", simulationResult.averageMemoryUtilization)}"
      )
    } else {
      log(s"\t${architecture.arrayConfig.dataflow}, " +
        s"${architecture.arrayConfig.asArrayDimension.arrayDimensionString}, " +
        s"${simulationResult.cycle}, " +
        s"${String.format("%.2f", simulationResult.areaUm2.get)}, " +
        s"${String.format("%.2f", simulationResult.energyPj.get)}, " +
        s"${String.format("%.6f", simulationResult.edap.get)}, " +
        s"${architecture.streamingDimensionSize}, " +
        s"${architecture.singleBufferLimitKbA}, " +
        s"${architecture.singleBufferLimitKbB}, " +
        s"${architecture.singleBufferLimitKbC}," +
        s"${String.format("%.2f", simulationResult.averageMemoryUtilization)}"
      )
    }
  }

  private def logOptimizationSummary(): Unit = {
    log("\n[Optimization Summary]")

    // 1. Summary of the optimization process
    log("\t[Optimization Process Summary]")
    log(s"\t\tStarting architectures: ${architectureCandidates.size}")
    log(s"\t\tExecutable architectures: ${executableCandidates.size}")
    log(s"\t\t[Process 1] Simulated architectures: ${rankedCalculatedResults.size}, Margin percent: $processOneMargin %")
    log(s"\t\t[Process 2] SRAM optimized architectures: ${archOptimizedResults.size}, Margin percent: $processTwoMargin %")
    log(s"\t\t[Process 3] Single SRAM optimized architectures: ${rankedSingleSramOptimizedResults.size}, Margin percent: $processThreeMargin %")
    log(s"\t\tFinal ranked architectures: ${rankedSingleSramOptimizedResults.size}")


    log("\n\t[Overall Optimization Improvement]")

    val initialBest = rankedCalculatedResults.minBy(_.simulationResult.cycle)
    val finalBest = rankedSingleSramOptimizedResults.head

    val cycleImprovement = (initialBest.simulationResult.cycle - finalBest.simulationResult.cycle).toDouble /
      initialBest.simulationResult.cycle * 100

    log(s"\t\tCycle time: ${String.format("%.2f", cycleImprovement)} % improvement")

    val avgSramSizeBefore = (initialBest.architecture.singleBufferLimitKbA +
      initialBest.architecture.singleBufferLimitKbB +
      initialBest.architecture.singleBufferLimitKbC) / 3.0

    val avgSramSizeAfter = (finalBest.architecture.singleBufferLimitKbA +
      finalBest.architecture.singleBufferLimitKbB +
      finalBest.architecture.singleBufferLimitKbC) / 3.0

    val sramReduction = (avgSramSizeBefore - avgSramSizeAfter) / avgSramSizeBefore * 100
    log(s"\t\tAverage SRAM size: ${String.format("%.2f", sramReduction)} % reduction")

    if (initialBest.architecture.streamingDimensionSize != finalBest.architecture.streamingDimensionSize) {
      val streamDimChange = (initialBest.architecture.streamingDimensionSize - finalBest.architecture.streamingDimensionSize).toDouble /
        initialBest.architecture.streamingDimensionSize * 100
      log(s"\t\tHalving streaming dimension happened streaming dimension: ${String.format("%.2f", streamDimChange)} % reduction")
    }

    val initialAvgUtilization = initialBest.simulationResult.averageMemoryUtilization
    val finalAvgUtilization = finalBest.simulationResult.averageMemoryUtilization

    val utilizationImprovement = finalAvgUtilization - initialAvgUtilization
    log(s"\t\tAverage memory utilization ${String.format("%.2f", utilizationImprovement)} % increase")

    if (initialBest.simulationResult.energyPj.isDefined && finalBest.simulationResult.energyPj.isDefined) {
      val energyImprovement = (initialBest.simulationResult.energyPj.get - finalBest.simulationResult.energyPj.get) /
        initialBest.simulationResult.energyPj.get * 100

      log(s"\t\tEnergy: ${String.format("%.2f", energyImprovement)} % improvement")
    }

    if (initialBest.simulationResult.areaUm2.isDefined && finalBest.simulationResult.areaUm2.isDefined) {
      val areaImprovement = (initialBest.simulationResult.areaUm2.get - finalBest.simulationResult.areaUm2.get) /
        initialBest.simulationResult.areaUm2.get * 100

      log(s"\t\tArea: ${String.format("%.2f", areaImprovement)} % improvement")
    }


    log("")

  }

  private def checkInitialStreamingDimensions(archBuffer: ArrayBuffer[Architecture]): ArrayBuffer[Architecture] = {

    val executableArchBuffer = ArrayBuffer.empty[Architecture]

    archBuffer.foreach { arch =>
      var currentArch = arch
      var success = false
      var attempts = 0
      val maxAttempts = 10

      while (!success && attempts < maxAttempts) {

        val layer = new Layer(
          layerName = simConfig.layerName,
          gemmDimension = simConfig.layerGemmDimension,
          arrayConfig = currentArch.arrayConfig,
          streamingDimensionSize = currentArch.streamingDimensionSize,
          offChipMemoryUploadOrder = currentArch.offChipMemoryUploadOrder,
          loggerOption = loggerOption
        )

        val tileSizeA = layer.operationVector.head.generateTileA.dims.memorySize
        val tileSizeB = layer.operationVector.head.generateTileB.dims.memorySize
        val tileSizeC = layer.operationVector.head.generateTileC.dims.memorySize

        buildSrams(
          arrayConfig = currentArch.arrayConfig,
          simConfig = simConfig,
          singleBufferLimitKbA = currentArch.singleBufferLimitKbA,
          singleBufferLimitKbB = currentArch.singleBufferLimitKbB,
          singleBufferLimitKbC = currentArch.singleBufferLimitKbC,
          tileSizeA = tileSizeA,
          tileSizeB = tileSizeB,
          tileSizeC = tileSizeC,
          loggerOption = loggerOption,
        ) match {
          case Right(_) =>
            success = true
            executableArchBuffer += currentArch

          case Left(_) =>
            if (currentArch.streamingDimensionSize > 1) {

//              log(s"\t\t\tStreaming Dimension ${currentArch.streamingDimensionSize} is too high to build SRAM")
              val newStreamingDimSize = Math.max(1, currentArch.streamingDimensionSize / 2)

//              log(s"\t\t\thalf the streaming dimension as $newStreamingDimSize\n")
              currentArch = currentArch.withStreamingDimensionSize(newStreamingDimSize)
              attempts += 1

            } else {
              attempts = maxAttempts
            }
        }

      }

      if (!success) {
        log(s"\t\tFailed to build arch ${arch.arrayConfig.arrayConfigString}")
        log(s"\t\tInitial streaming dimension is too high change max attempts current maximum attempts is $maxAttempts")
        log("")
      }

    }

    executableArchBuffer

  }

  private def evaluateAllArchitectures(architectureBuffer: ArrayBuffer[Architecture]): ArrayBuffer[ArchitectureResult] = {
    val parallelResults = architectureBuffer.par.map { arch =>
      buildAndRunSimulation(architecture = arch)
    }

    // Safely convert back to ArrayBuffer
    val resultBuffer = new ArrayBuffer[ArchitectureResult]()
    resultBuffer ++= parallelResults.seq
    resultBuffer

  }


  private def optimizeSramStreamingTradeOffs(
    archResultBuffer: ArrayBuffer[ArchitectureResult],
    processMargin: Double
  ): ArrayBuffer[ArchitectureResult] = {
    val maxIterations = 10
    var iteration = 0
    var globalMadeProgress = true

    case class ArchitectureState(
      archResult: ArchitectureResult,
      canOptimize: Boolean = true,
      madeProgress: Boolean = false
    )

    var architectureStates = archResultBuffer.map(ArchitectureState(_))
    var previousResults = archResultBuffer.clone()

    while(iteration < maxIterations && globalMadeProgress){

      globalMadeProgress = false

      val parallelStates = architectureStates.par.map { state =>

        if(state.canOptimize){
          val attemptResult = trySingleOptimizationStep(state.archResult)
          val improved = isImproved(currentResult = state.archResult.simulationResult, newResult = attemptResult.simulationResult)

          if(improved){
            ArchitectureState(attemptResult, madeProgress = true)
          } else {
            ArchitectureState(state.archResult, canOptimize = false)
          }

        } else {
          ArchitectureState(state.archResult, canOptimize = false)
        }

      }

      architectureStates = ArrayBuffer.empty[ArchitectureState]
      architectureStates ++= parallelStates.seq

      globalMadeProgress = architectureStates.exists(_.madeProgress)

      val currentResults = architectureStates.map(_.archResult)
      val rankedResults = rankResults(currentResults, processMargin)

      logProcessTwo(
        preProcess = previousResults,
        postProcess = currentResults,
        rankedPostProcess = rankedResults,
        iteration = iteration
      )

      // Keep only the states for architectures that made it through ranking
      val rankedArchIds = rankedResults.map(_.architecture).toSet
      architectureStates = architectureStates.filter(state =>
        rankedArchIds.contains(state.archResult.architecture))

      previousResults = rankedResults.clone()
      iteration += 1
    }

    val result = architectureStates.map(_.archResult)
//    logStatistics(result)
    log("")
    result

  }

  private def isImproved(currentResult: SimulationResult, newResult: SimulationResult): Boolean = {
    simConfig.metric match {
      case SystemArchitectureOptimizer.OptimizationMetric.Cycle =>
        newResult.cycle < currentResult.cycle
      case SystemArchitectureOptimizer.OptimizationMetric.Energy =>
        newResult.energyPj.exists( e=>
          currentResult.energyPj.exists(ce => e < ce)
        )
      case SystemArchitectureOptimizer.OptimizationMetric.Area =>
        newResult.areaUm2.exists( a =>
          currentResult.areaUm2.exists(ca => a < ca)
        )
      case SystemArchitectureOptimizer.OptimizationMetric.EDAP =>
        newResult.edap.exists(a =>
          currentResult.edap.exists(ca => a < ca)
        )
    }
  }

  private def trySingleOptimizationStep(currentArchResult: ArchitectureResult): ArchitectureResult = {

    val currentArch = currentArchResult.architecture

    if(currentArch.singleBufferLimitKbA <= minSramSize){
//      log(s"\t\t\tCannot reduce SRAM size further (minimum reached: $minSramSize")
      return currentArchResult
    }

    val nextSramSize = math.max(minSramSize, currentArch.singleBufferLimitKbA/2)
    assert(
      (currentArch.singleBufferLimitKbA == currentArch.singleBufferLimitKbB)
        && (currentArch.singleBufferLimitKbB == currentArch.singleBufferLimitKbC),
      "[error] Single buffer size if different"
    )

    val sramReducedArch = currentArch.withUniformSramSizes(nextSramSize)

//    log(s"\t\t\tTrying to reduce SRAM size from ${currentArch.singleBufferLimitKbA} KB to $nextSramSize KB")

    val sramReducedArchResult = buildAndRunSimulation(sramReducedArch)
    val isSramReductionValid = sramReducedArchResult.simulationResult.cycle != Long.MaxValue

    if(isSramReductionValid) {
      sramReducedArchResult
    } else {
//      log("\t\t\tSRAM reduction failed to build. Trying combined reduction next")
      if(currentArch.streamingDimensionSize <= 1){
//        log(s"\t\t\tCannot reduce streaming dimension further (minimum reached: 1")
        currentArchResult
      } else {
        val nextStreamingDimensionSize = math.max(1, currentArch.streamingDimensionSize / 2)
        val combinedReducedArch = currentArch
          .withUniformSramSizes(nextSramSize)
          .withStreamingDimensionSize(nextStreamingDimensionSize)

        val combinedReducedArchResult = buildAndRunSimulation(combinedReducedArch)
        val isCombinedResultValid = combinedReducedArchResult.simulationResult.cycle != Long.MaxValue

        if(isCombinedResultValid){
          combinedReducedArchResult
        } else {
          currentArchResult
        }
      }
    }
  }

  private def halfSingleSramSizes(
    archResultBuffer: ArrayBuffer[ArchitectureResult],
  ): ArrayBuffer[ArchitectureResult] = {

    @tailrec
    def halfSingleSramSize(currentEval: ArchitectureResult): ArchitectureResult= {

      val currentArch  = currentEval.architecture
      val currentSingleSramSize = currentArch.arrayConfig.dataflow match {
        case Dataflow.Is => currentArch.singleBufferLimitKbA
        case Dataflow.Os => currentArch.singleBufferLimitKbC
        case Dataflow.Ws => currentArch.singleBufferLimitKbB
      }
      val currentSimulationResult = currentEval.simulationResult

      if(currentSingleSramSize <= minSramSize){
//        log(s"\t\t\t${currentArch.arrayConfig.arrayConfigString} cannot reduce SRAM size further (minimum reached: ${minSramSize}KB)")
        return currentEval
      }

      val nextSramSize = currentSingleSramSize / 2

      val nextArchitecture = currentArch.arrayConfig.dataflow match {
        case Dataflow.Is => currentArch.withSramBufferSize(nextSramSize, DataType.A)
        case Dataflow.Os => currentArch.withSramBufferSize(nextSramSize, DataType.C)
        case Dataflow.Ws => currentArch.withSramBufferSize(nextSramSize, DataType.B)
      }

      val newEval = buildAndRunSimulation(nextArchitecture)

      if(isImproved(currentResult = currentSimulationResult, newResult  = newEval.simulationResult)){
        halfSingleSramSize(newEval)
      } else {
        currentEval
      }

    }

//    archResultBuffer.par.map(halfSingleSramSize).to(ArrayBuffer)
    val parallelResults = archResultBuffer.par.map(halfSingleSramSize)

    val resultBuffer = ArrayBuffer.empty[ArchitectureResult]
    resultBuffer ++= parallelResults.seq

    resultBuffer

  }

  private def rankResults(
    results: ArrayBuffer[ArchitectureResult],
    marginPercent: Double,
  ): ArrayBuffer[ArchitectureResult] = {

    val metric = simConfig.metric

    val threshold = metric match {
      case SystemArchitectureOptimizer.OptimizationMetric.Cycle =>
        val minCycle = results.map(_.simulationResult.cycle).min
        (minCycle * (1 + marginPercent/100)).toLong
      case SystemArchitectureOptimizer.OptimizationMetric.Energy =>
        val minEnergy = results.flatMap(_.simulationResult.energyPj).min
        minEnergy * (1 + marginPercent/100)
      case SystemArchitectureOptimizer.OptimizationMetric.Area =>
        val minArea = results.flatMap(_.simulationResult.areaUm2).min
        minArea * (1 + marginPercent/100)
      case SystemArchitectureOptimizer.OptimizationMetric.EDAP =>
        val minAreaEnergyProduct = results.flatMap(_.simulationResult.edap).min
        minAreaEnergyProduct * (1 + marginPercent/100)
    }

    val filteredResults = results.filter { result =>
      metric match {
        case SystemArchitectureOptimizer.OptimizationMetric.Cycle =>
          result.simulationResult.cycle <= threshold
        case SystemArchitectureOptimizer.OptimizationMetric.Energy =>
          result.simulationResult.energyPj.exists(_ <= threshold)
        case SystemArchitectureOptimizer.OptimizationMetric.Area =>
          result.simulationResult.areaUm2.exists(_ <= threshold)
        case SystemArchitectureOptimizer.OptimizationMetric.EDAP =>
          result.simulationResult.edap.exists(_ <= threshold)
      }
    }

    val sortedResults = metric match {
      case SystemArchitectureOptimizer.OptimizationMetric.Cycle =>
        filteredResults.sortBy(_.simulationResult.cycle)
      case SystemArchitectureOptimizer.OptimizationMetric.Energy =>
        filteredResults.sortBy(_.simulationResult.energyPj.get)
      case SystemArchitectureOptimizer.OptimizationMetric.Area =>
        filteredResults.sortBy(_.simulationResult.areaUm2.get)
      case SystemArchitectureOptimizer.OptimizationMetric.EDAP =>
        filteredResults.sortBy(_.simulationResult.edap.get)
    }

    sortedResults

  }

  private def buildAndRunSimulation(
    architecture: Architecture
  ): ArchitectureResult = {
    buildSimulationComp(architecture) match {
      case Right(comp) =>
        val result = runSimulation(comp)
        ArchitectureResult(architecture = architecture, simulationResult = result)
      case Left(_) =>
        ArchitectureResult(architecture = architecture, simulationResult = SimulationResult(Long.MaxValue, Double.MaxValue, Double.MaxValue))
    }
  }
//
  private def buildSrams(
    arrayConfig: ArrayConfig,
    simConfig: SystemArchitectureOptimizer.SimulationConfig,
    singleBufferLimitKbA: Int,
    singleBufferLimitKbB: Int,
    singleBufferLimitKbC: Int,
    tileSizeA: Int,
    tileSizeB: Int,
    tileSizeC: Int,
    loggerOption: LoggerOption
  ): Either[SramBuildError,(DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram)] = {

    val capacityA = singleBufferLimitKbA * 8 * 1024 / tileSizeA
    val capacityB = singleBufferLimitKbB * 8 * 1024 / tileSizeB
    val capacityC = singleBufferLimitKbC * 8 * 1024 / tileSizeC

    if(!(capacityA > 0 && capacityB > 0 && capacityC > 0 )) {
//        log(s"\t\tBuilding SRAM is failed: ${arrayConfig.arrayConfigString}\n" +
//          s"\t\t\tSRAM A Capacity: $capacityA, SRAM B Capacity: $capacityB, SRAM C Capacity: $capacityC\n" +
//          s"\t\t\tTile Size A: $tileSizeA, Tile Size B: $tileSizeB, Tile Size C: $tileSizeC")

      Left(SramBuildError("SRAM Cannot contain even 1 tile"))
    } else {
      val sramA = new DoubleBufferSram(
        dataType = DataType.A,
        outputBandwidth = arrayConfig.bandwidthOfInputA,
        singleBufferTileCapacity = capacityA,
        singleBufferLimitKb = singleBufferLimitKbA,
        loggerOption = loggerOption
      )
      val sramB = new DoubleBufferSram(
        dataType = DataType.B,
        outputBandwidth = arrayConfig.bandwidthOfInputB,
        singleBufferTileCapacity = capacityB,
        singleBufferLimitKb = singleBufferLimitKbB,
        loggerOption = loggerOption
      )
      val sramC = new OutputDoubleBufferSram(
        outputBandwidth = simConfig.offChipMemoryBandwidth,
        singleBufferTileCapacity = capacityC,
        singleBufferLimitKb = singleBufferLimitKbC,
        loggerOption = loggerOption
      )
      Right((sramA, sramB, sramC))
    }
  }

  private def buildSimulationComp(
    architecture: Architecture
  ): Either[SramBuildError,(Layer, OffChipMemory, Array, (DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram), Interface)] = {

    val layer = new Layer(
      layerName = simConfig.layerName,
      gemmDimension = simConfig.layerGemmDimension,
      arrayConfig = architecture.arrayConfig,
      streamingDimensionSize = architecture.streamingDimensionSize,
      offChipMemoryUploadOrder = architecture.offChipMemoryUploadOrder,
      loggerOption = loggerOption
    )

    val offChipMemory = new OffChipMemory(
      outputBandwidth = simConfig.offChipMemoryBandwidth,
      referenceData = simConfig.offChipMemoryReferenceData,
      loggerOption = loggerOption
    )

    val array = new Array(
      arrayConfig = architecture.arrayConfig,
      loggerOption = loggerOption
    )

    buildSrams(
      arrayConfig = architecture.arrayConfig,
      simConfig = simConfig,
      singleBufferLimitKbA = architecture.singleBufferLimitKbA,
      singleBufferLimitKbB = architecture.singleBufferLimitKbB,
      singleBufferLimitKbC = architecture.singleBufferLimitKbC,
      tileSizeA = layer.operationVector.head.generateTileA.dims.memorySize,
      tileSizeB = layer.operationVector.head.generateTileB.dims.memorySize,
      tileSizeC = layer.operationVector.head.generateTileC.dims.memorySize,
      loggerOption = loggerOption
    ) match {
      case Right(srams@(sramA, sramB, sramC)) =>
        val interface = new Interface(
          offChipMemory = offChipMemory,
          sramA = sramA,
          sramB = sramB,
          sramC = sramC,
          array = array
        )
        Right((layer, offChipMemory, array, srams, interface))

      case Left(error) =>
        Left(error)
    }

  }


  private def runSimulation(
    components: (Layer, OffChipMemory, Array, (DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram), Interface)
  ): SimulationResult = {

    val (layer, offChipMemory, array, srams, interface)= components
    val (sramA, sramB, sramC) = srams


    try {

      val simulation = new SystemSimulator(
        offChipMemory = offChipMemory,
        sramA = sramA,
        sramB = sramB,
        sramC = sramC,
        interface = interface,
        layer = layer,
        array = array,
        sramReferenceDataVector = simConfig.sramReferenceDataVector,
        loggerOption = loggerOption,
      )

      simulation.startSimulation()

      SimulationResult(
        totalOperationNumber = simulation.getTotalOperationNumber,
        tileSizeA = simulation.getTileSizeA,
        tileSizeB = simulation.getTileSizeB,
        tileSizeC = simulation.getTileSizeC,
        skipTileCountA = simulation.getSkipTileCountA,
        skipTileCountB = simulation.getSkipTileCountB,
        singleBufferTileCapacityA = simulation.getSingleBufferTileCapacityA,
        singleBufferTileCapacityB = simulation.getSingleBufferTileCapacityB,
        singleBufferTileCapacityC = simulation.getSingleBufferTileCapacityC,

        arrayInputBandwidthA = simulation.getArrayInputBandwidthA,
        arrayInputBandwidthB = simulation.getArrayInputBandwidthB,
        arrayOutputBandwidthC = simulation.getArrayOutputBandwidthC,

        arrayCapacityA = simulation.getArrayCapacityA,
        arrayCapacityB = simulation.getArrayCapacityB,

        cycle = simulation.getTotalCycle,
        arrayActiveCount = simulation.getArrayActiveCount,

        offChipMemoryReadAccessCount = simulation.getOffChipMemoryReadAccessCount,
        offChipMemoryWriteAccessCount = simulation.getOffChipMemoryWriteAccessCount,
        sramReadAccessCountA = simulation.getSramReadAccessCountA,
        sramWriteAccessCountA = simulation.getSramWriteAccessCountA,
        sramReadAccessCountB = simulation.getSramReadAccessCountB,
        sramWriteAccessCountB = simulation.getSramWriteAccessCountB,

        offChipMemoryHitRatio = simulation.getTotalOffChipMemoryHitCount,
        offChipMemoryMissRatio = simulation.getTotalOffChipMemoryMissCount,

        sramHitRatioA = simulation.getSramHitRatioA,
        sramHitRatioB = simulation.getSramHitRatioB,
        sramMissRatioA = simulation.getSramMissRatioA,
        sramMissRatioB = simulation.getSramMissRatioB,
        sramHitRatio = simulation.getTotalSramHitRatio,
        sramMissRatio = simulation.getTotalSramMissRatio,

        offChipMemoryStallCount = simulation.getOffChipMemoryStallCount,

        firstFillUpCycleA = simulation.getFirstFillUptCycleA,
        bufferSwapCountA = simulation.getBufferSwapCountA,
        bufferSwapStallCountA = simulation.getBufferSwapStallCountA,

        firstFillUpCycleB = simulation.getFirstFillUptCycleB,
        bufferSwapCountB = simulation.getBufferSwapCountB,
        bufferSwapStallCountB = simulation.getBufferSwapStallCountB,

        firstFillUpCycleC = simulation.getFirstFillUptCycleC,
        bufferSwapCountC = simulation.getBufferSwapCountC,
        bufferSwapStallCountC = simulation.getBufferSwapStallCountC,

        averageMemoryUsageKbA = simulation.getAverageMemoryUsageKbA,
        averageMemoryUtilizationA = simulation.getAverageMemoryUtilizationA,
        averageMemoryUsageKbB = simulation.getAverageMemoryUsageKbB,
        averageMemoryUtilizationB = simulation.getAverageMemoryUtilizationB,
        averageMemoryUsageKbC = simulation.getAverageMemoryUsageKbC,
        averageMemoryUtilizationC = simulation.getAverageMemoryUtilizationC,

        averageMemoryUtilization = simulation.getAverageMemoryUtilization,

        offChipMemoryReferenceData = simulation.getOffChipMemoryRefData,
        sramModelDataTable = simulation.getSramModelDataTable,
        arraySynthesisSource = simulation.getArraySynthesisSource,
        arraySynthesisData = simulation.getArraySynthesisData,

        sramReadEnergyPjA = simulation.getSramReadEnergyA,
        sramWriteEnergyPjA = simulation.getSramWriteEnergyA,
        sramLeakageEnergyPjA = simulation.getSramLeakageEnergyA,
        sramEnergyPjA = simulation.getSramEnergyA,

        sramReadEnergyPjB = simulation.getSramWriteEnergyB,
        sramWriteEnergyPjB = simulation.getSramWriteEnergyB,
        sramLeakageEnergyPjB = simulation.getSramLeakageEnergyB,
        sramEnergyPjB = simulation.getSramEnergyB,

        sramReadEnergyPjC = simulation.getSramReadEnergyC,
        sramWriteEnergyPjC = simulation.getSramWriteEnergyC,
        sramLeakageEnergyPjC = simulation.getSramLeakageEnergyC,
        sramEnergyPjC = simulation.getSramEnergyC,

        offChipMemoryReadEnergyPj = simulation.getOffChipMemoryReadEnergy,
        offChipMemoryWriteEnergyPj = simulation.getOffChipMemoryWriteEnergy,
        offChipMemoryEnergyPj = simulation.getOffChipMemoryEnergy,

        arrayDynamicEnergyPj = simulation.getArrayDynamicEnergy,
        arrayLeakageEnergyPj = simulation.getArrayLeakageEnergy,
        arrayEnergy = simulation.getArrayEnergy,

        energyPj = simulation.getTotalEnergy,

        sramAreaUm2A = simulation.getSramAreaA,
        sramAreaUm2B = simulation.getSramAreaB,
        sramAreaUm2C = simulation.getSramAreaC,

        arrayAreaUm2 = simulation.getArrayArea,
        areaUm2 = simulation.getArea,

        edap = simulation.calculateEDAP

      )


    } catch {
      case _: RunTimeError =>
        Console.err.println("Program Wrong")
        sys.exit(1)

      case error: SramBuildError =>
        log(s"\t\t$error")
        SimulationResult(Long.MaxValue, Double.MaxValue, Double.MaxValue)
    }

  }

}

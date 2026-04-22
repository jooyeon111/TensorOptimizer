package simulation

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import common.Dataflow
import scala.collection.parallel.CollectionConverters._

class ArchitectureOptimizerBo(
                               val simConfig: SystemArchitectureOptimizerBo.SimulationConfig,
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

  // Cache simulation results to avoid redundant evaluations across BO candidates
  private val simulationCache = new java.util.concurrent.ConcurrentHashMap[(String, Int, Int), ArchitectureResult]()

  private val processOneMargin: Double = 20.0
  private val processTwoMargin: Double = 40.0
  private val processThreeMargin: Double = 10.0

  def run(): Unit = {

    log("[Optimization Process]")
    executableCandidates ++= validateCandidates(architectureCandidates)
    logExecutableArch(
      candidates = architectureCandidates,
      validCandidates = executableCandidates,
    )

    calculatedAllResults ++= process1(executableCandidates)
    rankedCalculatedResults ++= rankResults(calculatedAllResults, processOneMargin)
    logProcessOne(
      validCandidates = executableCandidates,
      simulatedCandidates = calculatedAllResults,
      rankedSimulatedCandidates = rankedCalculatedResults,
    )

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
    log(s"[Show Top Results CSV Format (" +
      s"Dataflow, " +
      s"STA Config, " +
      s"Cycle, " +
      s"Area, " +
      s"Energy, " +
      s"TOPS/W/mm^2, " +
      s"Streaming Dimension, " +
      s"Single Buffer Size A, " +
      s"Single Buffer Size B, " +
      s"Single Buffer Size C, " +
      s"Memory Utilization]")
    rankedSingleSramOptimizedResults.foreach(logCsv)
    log("")
  }

  def logTopEasyResultsCsv(): Unit = {
    log(s"[Show Top Results CSV Format (" +
      s"Dataflow, " +
      s"STA Config)] ")
    rankedSingleSramOptimizedResults.foreach(logEasyCsv)
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

    log(s"\t\t[Iteration ${iteration+1} Result]")
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

      //      if (preProcessBest.simulationResult.tops.isDefined && postProcessBest.simulationResult.tops.isDefined) {
      //        val energyAreaProductImprovement= (preProcessBest.simulationResult.tops.get - postProcessBest.simulationResult.tops.get) /
      //          preProcessBest.simulationResult.tops.get * 100
      //        log(s"\t\t\tArea-Energy-Delay Product: ${String.format("%.6f", energyAreaProductImprovement)}% improvement")
      //      }
      if (preProcessBest.simulationResult.tops.isDefined && postProcessBest.simulationResult.tops.isDefined) {
        val oldValue = preProcessBest.simulationResult.tops.get
        val newValue = postProcessBest.simulationResult.tops.get

        if (oldValue <= 0 || newValue <= 0) {
          log(s"\t\t\tWarning: Invalid efficiency values - old: $oldValue, new: $newValue")
        } else {
          val improvement = (newValue - oldValue) / oldValue * 100
          log(s"\t\t\tTOPS/W/mm² Efficiency: ${String.format("%.2f", improvement)}% improvement")
        }
      }
    }

    log("")

  }

  //TODO fix log
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

    if (preProcessBest.simulationResult.tops.isDefined && postProcessBest.simulationResult.tops.isDefined) {
      val oldValue = preProcessBest.simulationResult.tops.get
      val newValue = postProcessBest.simulationResult.tops.get

      if (oldValue <= 0 || newValue <= 0) {
        log(s"\t\t\tWarning: Invalid efficiency values - old: $oldValue, new: $newValue")
      } else {
        val improvement = (newValue - oldValue) / oldValue * 100
        log(s"\t\t\tTOPS/W/mm² Efficiency: ${String.format("%.2f", improvement)}% improvement")
      }
    }

    //    logStatistics(rankedPostProcess)

  }

  private def logSummary(ArchitectureResult: ArchitectureResult): Unit = {
    val simulationResult = ArchitectureResult.simulationResult
    val architecture = ArchitectureResult.architecture

    if(simulationResult.isEnergyReportValid && simulationResult.isAreaReportValid){
      log(s"\t[${architecture.arrayConfig.arrayConfigString}]")
      log(s"\t\tArray Active Cycle: ${simulationResult.arrayActiveCount}")
      log(s"\t\tCycle: ${simulationResult.cycle}")
      log(s"\t\tArea: ${String.format("%.2f", simulationResult.areaUm2.get)} um²")
      log(s"\t\tEnergy: ${String.format("%.2f", simulationResult.energyPj.get)} pJ")
      log(s"\t\tTOPS/W/mm^2: ${String.format("%.2f", simulationResult.tops.get)}")
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
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramA.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramA.bandwidthBits}")

      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramA.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramA.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramA.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramA.areaUm2} um^2\n")

      log(s"")
      log(s"\t\t\t[Weight SRAM B]")
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramB.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramB.bandwidthBits}")
      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramB.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramB.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramB.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramB.areaUm2} um^2\n")

      log(s"")
      log(s"\t\t\t[Output SRAM C]")
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramC.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramC.bandwidthBits}")
      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramC.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramC.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramC.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramC.areaUm2} um^2\n")


    } else {
      log(s"\t[${architecture.arrayConfig.arrayConfigString}]")
      log(s"\t\tArray Active Cycle: ${simulationResult.arrayActiveCount}")
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
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramA.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramA.bandwidthBits}")
      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramA.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramA.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramA.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramA.areaUm2} um^2\n")

      log(s"")
      log(s"\t\t\t[Weight SRAM B]")
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramB.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramB.bandwidthBits}")
      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramB.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramB.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramB.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramB.areaUm2} um^2\n")

      log(s"")
      log(s"\t\t\t[Output SRAM C]")
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramC.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramC.bandwidthBits}")
      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramC.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramC.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramC.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramC.areaUm2} um^2\n")


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
        s"${String.format("%.6f", simulationResult.tops.get)}, " +
        s"${architecture.streamingDimensionSize}, " +
        s"${architecture.singleBufferLimitKbA}, " +
        s"${architecture.singleBufferLimitKbB}, " +
        s"${architecture.singleBufferLimitKbC}, " +
        s"${String.format("%.2f", simulationResult.averageMemoryUtilization)}"
      )
    } else {
      log(s"\t${architecture.arrayConfig.dataflow}, " +
        s"${architecture.arrayConfig.asArrayDimension.arrayDimensionString}, " +
        s"${simulationResult.arrayActiveCount}, " +
        s"${simulationResult.cycle}, " +
        s"${String.format("%.2f", simulationResult.areaUm2.get)}, " +
        s"${String.format("%.2f", simulationResult.energyPj.get)}, " +
        s"${String.format("%.6f", simulationResult.tops.get)}, " +
        s"${architecture.streamingDimensionSize}, " +
        s"${architecture.singleBufferLimitKbA}, " +
        s"${architecture.singleBufferLimitKbB}, " +
        s"${architecture.singleBufferLimitKbC}," +
        s"${String.format("%.2f", simulationResult.averageMemoryUtilization)}"
      )
    }
  }

  private def logEasyCsv(ArchitectureResult: ArchitectureResult): Unit = {

    val simulationResult = ArchitectureResult.simulationResult
    val architecture = ArchitectureResult.architecture

    if(simulationResult.isEnergyReportValid && simulationResult.isAreaReportValid) {
      log(s"\t${architecture.arrayConfig.dataflow} " +
        s"${architecture.arrayConfig.asArrayDimension.arrayDimensionString}"
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
              val newStreamingDimSize = Math.max(1, currentArch.streamingDimensionSize / 2)
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
    val resultBuffer = new ArrayBuffer[ArchitectureResult]()
    resultBuffer ++= parallelResults.seq
    resultBuffer
  }

  // ============================================================
  // Bayesian Optimization for Process 2 (replaces iterative halving)
  // ============================================================

  /**
   * Observed data point for the Gaussian Process surrogate model.
   * Parameters are encoded as log2 values of the power-of-2 SRAM/streaming sizes.
   */
  private case class ObservedPoint(
                                    sramSizeLog2: Int,            // log2 of uniform SRAM size
                                    streamingDimLog2: Int,        // log2 of streaming dimension size
                                    objectiveValue: Double,       // the metric value (lower is better for cycle/energy/area, higher for TOPS)
                                    archResult: ArchitectureResult,
                                    isValid: Boolean              // whether simulation succeeded
                                  )

  /**
   * Gaussian Process surrogate model for Bayesian optimization.
   * Improvements over naive implementation:
   *   - ARD (Automatic Relevance Determination) kernel: separate length scale per dimension
   *   - Higher noise variance for numerical stability on discrete spaces
   *   - Log marginal likelihood based hyperparameter optimization
   *   - Cholesky decomposition for numerically stable solves
   */
  private class GaussianProcessSurrogate(
                                          private var lengthScales: scala.Array[Double] = scala.Array(1.5, 1.5),
                                          private var signalVariance: Double = 1.0,
                                          private var noiseVariance: Double = 0.1
                                        ) {
    private var observedX: ArrayBuffer[scala.Array[Double]] = ArrayBuffer.empty
    private var observedY: ArrayBuffer[Double] = ArrayBuffer.empty
    private var yMean: Double = 0.0
    private var yStd: Double = 1.0

    // Cached Cholesky factor and alpha vector, invalidated on new observations
    private var cachedL: Option[scala.Array[scala.Array[Double]]] = None
    private var cachedAlpha: Option[scala.Array[Double]] = None

    def addObservation(x: scala.Array[Double], y: Double): Unit = {
      observedX += x
      observedY += y
      invalidateCache()
      updateNormalization()
      // Optimize hyperparameters every 5 observations to reduce overhead
      // (grid search over 324 combinations × O(N³) Cholesky per combo)
      if (observedX.size >= 4 && observedX.size % 5 == 0) optimizeHyperparameters()
    }

    def addObservations(xs: Seq[scala.Array[Double]], ys: Seq[Double]): Unit = {
      observedX ++= xs
      observedY ++= ys
      invalidateCache()
      updateNormalization()
      // Skip hyperparameter optimization during seed registration;
      // it will run naturally via addObservation every 5 steps in the BO loop
    }

    private def invalidateCache(): Unit = {
      cachedL = None
      cachedAlpha = None
    }

    private def updateNormalization(): Unit = {
      if (observedY.nonEmpty) {
        yMean = observedY.sum / observedY.size
        val variance = observedY.map(y => (y - yMean) * (y - yMean)).sum / observedY.size
        yStd = if (variance > 0) math.sqrt(variance) else 1.0
      }
    }

    private def normalizedY: scala.Array[Double] = observedY.map(y => (y - yMean) / yStd).toArray

    /** ARD RBF kernel: separate length scale per dimension */
    private def ardRbfKernel(
                              x1: scala.Array[Double],
                              x2: scala.Array[Double],
                              ls: scala.Array[Double],
                              sv: Double
                            ): Double = {
      var squaredDist = 0.0
      var i = 0
      while (i < x1.length) {
        val diff = (x1(i) - x2(i)) / ls(i)
        squaredDist += diff * diff
        i += 1
      }
      sv * math.exp(-0.5 * squaredDist)
    }

    private def rbfKernel(x1: scala.Array[Double], x2: scala.Array[Double]): Double = {
      ardRbfKernel(x1, x2, lengthScales, signalVariance)
    }

    /** Build kernel matrix K(X,X) + noise*I */
    private def buildKernelMatrix(
                                   ls: scala.Array[Double],
                                   sv: Double,
                                   nv: Double
                                 ): scala.Array[scala.Array[Double]] = {
      val n = observedX.size
      val K = scala.Array.ofDim[Double](n, n)
      for (i <- 0 until n) {
        for (j <- i until n) {
          val kij = ardRbfKernel(observedX(i), observedX(j), ls, sv)
          K(i)(j) = kij
          K(j)(i) = kij
        }
        K(i)(i) += nv
      }
      K
    }

    /**
     * Cholesky decomposition: returns lower triangular L such that A = L * L^T.
     * Returns None if matrix is not positive definite.
     */
    private def choleskyDecomposition(A: scala.Array[scala.Array[Double]]): Option[scala.Array[scala.Array[Double]]] = {
      val n = A.length
      val L = scala.Array.ofDim[Double](n, n)

      for (i <- 0 until n) {
        for (j <- 0 to i) {
          var sum = 0.0
          for (k <- 0 until j) {
            sum += L(i)(k) * L(j)(k)
          }
          if (i == j) {
            val diag = A(i)(i) - sum
            if (diag <= 0.0) return None
            L(i)(j) = math.sqrt(diag)
          } else {
            L(i)(j) = (A(i)(j) - sum) / L(j)(j)
          }
        }
      }
      Some(L)
    }

    /** Solve L * x = b where L is lower triangular (forward substitution) */
    private def solveForwardTriangular(L: scala.Array[scala.Array[Double]], b: scala.Array[Double]): scala.Array[Double] = {
      val n = b.length
      val x = scala.Array.fill(n)(0.0)
      for (i <- 0 until n) {
        var sum = b(i)
        for (j <- 0 until i) {
          sum -= L(i)(j) * x(j)
        }
        x(i) = sum / L(i)(i)
      }
      x
    }

    /** Solve L^T * x = b where L is lower triangular (back substitution) */
    private def solveBackwardTriangular(L: scala.Array[scala.Array[Double]], b: scala.Array[Double]): scala.Array[Double] = {
      val n = b.length
      val x = scala.Array.fill(n)(0.0)
      for (i <- (n - 1) to 0 by -1) {
        var sum = b(i)
        for (j <- i + 1 until n) {
          sum -= L(j)(i) * x(j)
        }
        x(i) = sum / L(i)(i)
      }
      x
    }

    /** Solve (L * L^T) * x = b using Cholesky factor */
    private def choleskySolve(L: scala.Array[scala.Array[Double]], b: scala.Array[Double]): scala.Array[Double] = {
      val z = solveForwardTriangular(L, b)
      solveBackwardTriangular(L, z)
    }

    /** Ensure cached L and alpha are up to date */
    private def ensureCache(): Unit = {
      if (cachedL.isEmpty) {
        val K = buildKernelMatrix(lengthScales, signalVariance, noiseVariance)
        cachedL = choleskyDecomposition(K)
        cachedL match {
          case Some(choleskyL) =>
            cachedAlpha = Some(choleskySolve(choleskyL, normalizedY))
          case None =>
            // Fallback: increase noise until decomposition succeeds
            var nv = noiseVariance * 10.0
            var attempt = 0
            while (cachedL.isEmpty && attempt < 5) {
              val Kretry = buildKernelMatrix(lengthScales, signalVariance, nv)
              cachedL = choleskyDecomposition(Kretry)
              nv *= 10.0
              attempt += 1
            }
            cachedL match {
              case Some(choleskyL) => cachedAlpha = Some(choleskySolve(choleskyL, normalizedY))
              case None => cachedAlpha = None
            }
        }
      }
    }

    /**
     * Predict mean and variance at a query point.
     * Returns (mean, variance) in the original (un-normalized) scale.
     */
    def predict(xQuery: scala.Array[Double]): (Double, Double) = {
      if (observedX.isEmpty) return (0.0, signalVariance)

      ensureCache()

      (cachedL, cachedAlpha) match {
        case (Some(choleskyL), Some(alpha)) =>
          val kStar = observedX.map(x => rbfKernel(x, xQuery)).toArray
          val kStarStar = rbfKernel(xQuery, xQuery) + noiseVariance

          val predictedMeanNorm = kStar.zip(alpha).map { case (k, a) => k * a }.sum

          val v = solveForwardTriangular(choleskyL, kStar)
          val predictedVariance = math.max(0.0, kStarStar - v.map(vi => vi * vi).sum)

          val predictedMean = predictedMeanNorm * yStd + yMean
          val scaledVariance = predictedVariance * yStd * yStd

          (predictedMean, scaledVariance)

        case _ =>
          // Fallback: return prior with high uncertainty
          (yMean, signalVariance * yStd * yStd)
      }
    }

    /**
     * Compute negative log marginal likelihood for hyperparameter optimization.
     * NLML = 0.5 * y^T K^{-1} y + 0.5 * log|K| + n/2 * log(2*pi)
     */
    private def negativeLogMarginalLikelihood(
                                               ls: scala.Array[Double],
                                               sv: Double,
                                               nv: Double
                                             ): Double = {
      val n = observedX.size
      if (n == 0) return Double.MaxValue

      val K = buildKernelMatrix(ls, sv, nv)
      val yNorm = normalizedY

      choleskyDecomposition(K) match {
        case Some(choleskyL) =>
          val alpha = choleskySolve(choleskyL, yNorm)

          // data fit term: 0.5 * y^T * alpha
          val dataFit = 0.5 * yNorm.zip(alpha).map { case (y, a) => y * a }.sum

          // complexity term: sum of log of diagonal of L = 0.5 * log|K|
          val logDet = choleskyL.indices.map(i => math.log(choleskyL(i)(i))).sum  // this is 0.5 * log|K|

          // constant term
          val constant = 0.5 * n * math.log(2.0 * math.Pi)

          dataFit + logDet + constant

        case None =>
          Double.MaxValue // not positive definite, reject this hyperparameter setting
      }
    }

    /**
     * Optimize GP hyperparameters by grid search over discrete candidates
     * in log space. Simple but robust for 2D problems with few hyperparameters.
     */
    private def optimizeHyperparameters(): Unit = {
      if (observedX.size < 4) return

      val lsCandidates = scala.Array(0.8, 1.5, 3.0, 5.0)
      val svCandidates = scala.Array(0.5, 1.0, 2.0)
      val nvCandidates = scala.Array(0.01, 0.1, 0.5)

      var bestNlml = Double.MaxValue
      var bestLs = lengthScales.clone()
      var bestSv = signalVariance
      var bestNv = noiseVariance

      for {
        ls0 <- lsCandidates
        ls1 <- lsCandidates
        sv <- svCandidates
        nv <- nvCandidates
      } {
        val ls = scala.Array(ls0, ls1)
        val nlml = negativeLogMarginalLikelihood(ls, sv, nv)
        if (nlml < bestNlml) {
          bestNlml = nlml
          bestLs = ls
          bestSv = sv
          bestNv = nv
        }
      }

      lengthScales = bestLs
      signalVariance = bestSv
      noiseVariance = bestNv
      invalidateCache()
    }

    def observationCount: Int = observedX.size
  }

  /**
   * Expected Improvement acquisition function with exploration jitter (xi).
   * xi > 0 encourages exploration by requiring improvement beyond the current best
   * by at least xi, preventing premature convergence.
   *
   * For minimization: EI(x) = (bestY - mu - xi) * Phi(z) + sigma * phi(z)
   * For maximization (TOPS): EI(x) = (mu - bestY - xi) * Phi(z) + sigma * phi(z)
   */
  private def expectedImprovement(
                                   mu: Double,
                                   variance: Double,
                                   bestObjective: Double,
                                   maximize: Boolean,
                                   xi: Double = 0.01
                                 ): Double = {
    val sigma = math.sqrt(math.max(variance, 1e-12))
    if (sigma < 1e-10) return 0.0

    val z = if (maximize) (mu - bestObjective - xi) / sigma
    else (bestObjective - mu - xi) / sigma

    val phiZ = standardNormalPdf(z)
    val cdfZ = standardNormalCdf(z)

    z * sigma * cdfZ + sigma * phiZ
  }

  private def standardNormalPdf(x: Double): Double = {
    math.exp(-0.5 * x * x) / math.sqrt(2.0 * math.Pi)
  }

  private def standardNormalCdf(x: Double): Double = {
    // Abramowitz and Stegun approximation
    0.5 * (1.0 + erf(x / math.sqrt(2.0)))
  }

  private def erf(x: Double): Double = {
    // Horner form approximation of the error function
    val sign = if (x >= 0) 1.0 else -1.0
    val absX = math.abs(x)
    val t = 1.0 / (1.0 + 0.3275911 * absX)
    val poly = t * (0.254829592 + t * (-0.284496736 + t * (1.421413741 + t * (-1.453152027 + t * 1.061405429))))
    sign * (1.0 - poly * math.exp(-absX * absX))
  }

  /**
   * Generate all candidate points in the discrete search space.
   * SRAM sizes and streaming dimensions are powers of 2.
   */
  private def generateCandidatePoints(
                                       maxSramLog2: Int,
                                       minSramLog2: Int,
                                       maxStreamDimLog2: Int,
                                       minStreamDimLog2: Int
                                     ): Seq[(Int, Int)] = {
    for {
      sramLog2 <- minSramLog2 to maxSramLog2
      streamLog2 <- minStreamDimLog2 to maxStreamDimLog2
    } yield (sramLog2, streamLog2)
  }

  /**
   * Extract the objective value from a simulation result based on the optimization metric.
   */
  private def extractObjective(result: SimulationResult): Double = {
    simConfig.metric match {
      case SystemArchitectureOptimizerBo.OptimizationMetric.Cycle => result.cycle.toDouble
      case SystemArchitectureOptimizerBo.OptimizationMetric.Energy => result.energyPj.getOrElse(Double.MaxValue)
      case SystemArchitectureOptimizerBo.OptimizationMetric.Area => result.areaUm2.getOrElse(Double.MaxValue)
      case SystemArchitectureOptimizerBo.OptimizationMetric.TOPS => result.tops.getOrElse(0.0)
    }
  }

  private def isMaximizing: Boolean = {
    simConfig.metric == SystemArchitectureOptimizerBo.OptimizationMetric.TOPS
  }

  /**
   * Compute a dynamic infeasible penalty based on observed valid objective values.
   * The penalty is set to be clearly worse than the worst observed valid result,
   * with a margin that scales with the observed range. This is robust to any
   * objective scale, including near-zero and negative values.
   */
  private def computeInfeasiblePenalty(validObjectives: Seq[Double], maximize: Boolean): Double = {
    if (validObjectives.isEmpty) return if (maximize) -1e12 else 1e12

    if (maximize) {
      // For maximization (TOPS): penalty should be much lower than worst observed
      val worst = validObjectives.min
      val range = validObjectives.max - worst
      val margin = math.max(math.abs(worst) * 0.5, range * 0.5) + 1.0
      worst - margin
    } else {
      // For minimization (cycle/energy/area): penalty should be much higher than worst observed
      val worst = validObjectives.max
      val range = worst - validObjectives.min
      val margin = math.max(math.abs(worst) * 0.5, range * 0.5) + 1.0
      worst + margin
    }
  }

  private def buildArchitectureFromParams(
                                           baseArch: Architecture,
                                           sramSizeLog2: Int,
                                           streamDimLog2: Int
                                         ): Architecture = {
    val sramSize = 1 << sramSizeLog2
    val streamDim = 1 << streamDimLog2

    val updatedArrayConfig = baseArch.arrayConfig.copy(
      arraySynthesisData = FewShotPredictor.predict(
        FewShotPredictor.InputFeatures(
          dataflow = baseArch.arrayConfig.dataflow.toString.toUpperCase,
          totalNumberOfMultipliers = baseArch.arrayConfig.totalNumberOfMultipliers,
          r = baseArch.arrayConfig.groupPeRow,
          c = baseArch.arrayConfig.groupPeCol,
          a = baseArch.arrayConfig.vectorPeRow,
          b = baseArch.arrayConfig.vectorPeCol,
          p = baseArch.arrayConfig.numMultiplier,
          streamingDimensionSize = streamDim
        )
      ).toOption
    )

    baseArch
      .withUniformSramSizes(sramSize)
      .withStreamingDimensionSize(streamDim)
      .copy(arrayConfig = updatedArrayConfig)
  }

  /**
   * Cached version of buildAndRunSimulation for BO process.
   * Uses (arrayConfigString, sramLog2, streamDimLog2) as cache key to avoid
   * redundant expensive simulations across different BO candidates.
   */
  private def cachedBuildAndRunSimulation(
                                           baseArch: Architecture,
                                           sramSizeLog2: Int,
                                           streamDimLog2: Int
                                         ): ArchitectureResult = {
    val key = (baseArch.arrayConfig.arrayConfigString, sramSizeLog2, streamDimLog2)
    val cached = simulationCache.get(key)
    if (cached != null) {
      cached
    } else {
      val arch = buildArchitectureFromParams(baseArch, sramSizeLog2, streamDimLog2)
      val result = buildAndRunSimulation(arch)
      simulationCache.putIfAbsent(key, result)
      result
    }
  }

  private def optimizeSramStreamingTradeOffs(
                                              archResultBuffer: ArrayBuffer[ArchitectureResult],
                                              processMargin: Double
                                            ): ArrayBuffer[ArchitectureResult] = {

    val maxBayesianIterations = 4
    val previousResults = archResultBuffer.clone()

    // Run Bayesian optimization independently for each architecture candidate
    val optimizedResults = archResultBuffer.par.map { initialResult =>
      bayesianOptimizeSingleArchitecture(initialResult, maxBayesianIterations)
    }

    //    val optimizedResults = archResultBuffer.par.map { initialResult =>
    //      try {
    //        bayesianOptimizeSingleArchitecture(initialResult, maxBayesianIterations)
    //      } catch {
    //        case e: Exception =>
    //          log(s"\t\t\tBayesian optimization failed: ${e.getMessage}")
    //          initialResult // fall back to the unoptimized result
    //      }
    //    }

    val resultBuffer = ArrayBuffer.empty[ArchitectureResult]
    resultBuffer ++= optimizedResults.seq

    val rankedResults = rankResults(resultBuffer, processMargin)

    logProcessTwo(
      preProcess = previousResults,
      postProcess = resultBuffer,
      rankedPostProcess = rankedResults,
      iteration = 0
    )

    log("")
    rankedResults
  }

  /**
   * Bayesian optimization loop for a single architecture.
   * Explores SRAM size and streaming dimension trade-offs using GP + EI.
   *
   * Improvements:
   *   - Infeasible points are added to GP with penalty values (not skipped)
   *   - Better initial seeding with more diverse points
   *   - Adaptive xi (jitter) that increases when stuck to encourage exploration
   *   - Higher early stopping patience
   */
  private def bayesianOptimizeSingleArchitecture(
                                                  initialResult: ArchitectureResult,
                                                  maxIterations: Int
                                                ): ArchitectureResult = {

    val baseArch = initialResult.architecture

    // Define search bounds in log2 space
    // Limit search range to max 3 steps from initial point to avoid
    // extreme SRAM/streaming combinations that cause very long simulations.
    // This mirrors RL behavior where step-by-step reduction + early stopping
    // naturally prevents reaching extreme configurations.
    val maxSramLog2 = (math.log(baseArch.singleBufferLimitKbA.toDouble) / math.log(2.0)).toInt
    val absoluteMinSramLog2 = (math.log(math.max(minSramSize, 1).toDouble) / math.log(2.0)).ceil.toInt
    val minSramLog2 = math.max(absoluteMinSramLog2, maxSramLog2 - 3)
    val maxStreamDimLog2 = (math.log(baseArch.streamingDimensionSize.toDouble) / math.log(2.0)).toInt
    val minStreamDimLog2 = math.max(0, maxStreamDimLog2 - 3)

    if (maxSramLog2 <= minSramLog2 && maxStreamDimLog2 <= minStreamDimLog2) {
      return initialResult
    }

    val gp = new GaussianProcessSurrogate()
    val allObserved = ArrayBuffer.empty[ObservedPoint]
    val evaluatedPoints = scala.collection.mutable.Set.empty[(Int, Int)]
    val maximize = isMaximizing

    // Register only the initial point (no extra seed simulation needed)
    val initSramLog2 = maxSramLog2
    val initStreamLog2 = maxStreamDimLog2
    val initObjective = extractObjective(initialResult.simulationResult)

    allObserved += ObservedPoint(initSramLog2, initStreamLog2, initObjective, initialResult, isValid = true)
    evaluatedPoints += ((initSramLog2, initStreamLog2))

    // Register initial point with GP
    gp.addObservations(
      Seq(scala.Array(initSramLog2.toDouble, initStreamLog2.toDouble)),
      Seq(initObjective)
    )

    // Generate all discrete candidate points
    val allCandidates = generateCandidatePoints(maxSramLog2, minSramLog2, maxStreamDimLog2, minStreamDimLog2)

    // Bayesian optimization loop with adaptive xi
    var iteration = 0
    var noImprovementCount = 0
    // Adaptive early stopping: scale patience with search space size
    val totalCandidates = allCandidates.size
    val maxNoImprovement = math.max(2, math.min(totalCandidates / 4, 5))
    val baseXi = 0.01

    while (iteration < maxIterations && noImprovementCount < maxNoImprovement) {

      // Find the best observed objective so far (among valid points only)
      val validObserved = allObserved.filter(_.isValid)
      if (validObserved.isEmpty) {
        return initialResult
      }

      val bestObjective = if (maximize) validObserved.map(_.objectiveValue).max
      else validObserved.map(_.objectiveValue).min

      // Recompute infeasible penalty dynamically as more valid points are observed
      val currentPenalty = computeInfeasiblePenalty(validObserved.map(_.objectiveValue).toSeq, maximize)

      // Adaptive xi: increase exploration when stuck
      val xi = baseXi * (1.0 + noImprovementCount * 0.5)

      // Evaluate EI for all unevaluated candidate points
      val unevaluatedCandidates = allCandidates.filterNot(p => evaluatedPoints.contains(p))

      if (unevaluatedCandidates.isEmpty) {
        log(s"\t\t\tBayesian optimization: exhausted search space after ${evaluatedPoints.size} evaluations")
        val bestPoint = validObserved.minBy(p => if (maximize) -p.objectiveValue else p.objectiveValue)
        return bestPoint.archResult
      }

      // Select the point with the highest EI
      val bestCandidate = unevaluatedCandidates.maxBy { case (sLog2, dLog2) =>
        val (mu, variance) = gp.predict(scala.Array(sLog2.toDouble, dLog2.toDouble))
        expectedImprovement(mu, variance, bestObjective, maximize, xi)
      }

      val (nextSramLog2, nextStreamLog2) = bestCandidate

      // Evaluate the selected point (with cache to avoid redundant simulations)
      val candidateResult = cachedBuildAndRunSimulation(baseArch, nextSramLog2, nextStreamLog2)
      val isValid = candidateResult.simulationResult.cycle != Long.MaxValue

      // Add ALL points to GP (including infeasible with dynamic penalty)
      val objective = if (isValid) extractObjective(candidateResult.simulationResult)
      else currentPenalty

      gp.addObservation(scala.Array(nextSramLog2.toDouble, nextStreamLog2.toDouble), objective)

      if (isValid) {
        val improved = if (maximize) objective > bestObjective else objective < bestObjective
        if (improved) {
          noImprovementCount = 0
        } else {
          noImprovementCount += 1
        }
      } else {
        noImprovementCount += 1
      }

      allObserved += ObservedPoint(nextSramLog2, nextStreamLog2, objective, candidateResult, isValid)
      evaluatedPoints += ((nextSramLog2, nextStreamLog2))

      iteration += 1
    }

    // Return the best observed result
    val validObserved = allObserved.filter(_.isValid)
    if (validObserved.isEmpty) {
      initialResult
    } else {
      val bestPoint = validObserved.minBy(p => if (maximize) -p.objectiveValue else p.objectiveValue)

      // Only return the optimized result if it's actually better than initial
      if (isImproved(currentResult = initialResult.simulationResult, newResult = bestPoint.archResult.simulationResult)) {
        bestPoint.archResult
      } else {
        initialResult
      }
    }
  }

  private def isImproved(currentResult: SimulationResult, newResult: SimulationResult): Boolean = {
    simConfig.metric match {
      case SystemArchitectureOptimizerBo.OptimizationMetric.Cycle =>
        newResult.cycle < currentResult.cycle
      case SystemArchitectureOptimizerBo.OptimizationMetric.Energy =>
        newResult.energyPj.exists( e=>
          currentResult.energyPj.exists(ce => e < ce)
        )
      case SystemArchitectureOptimizerBo.OptimizationMetric.Area =>
        newResult.areaUm2.exists( a =>
          currentResult.areaUm2.exists(ca => a < ca)
        )
      case SystemArchitectureOptimizerBo.OptimizationMetric.TOPS =>
        newResult.tops.exists(a =>
          currentResult.tops.exists(ca => a > ca)
        )
    }
  }

  // trySingleOptimizationStep has been replaced by bayesianOptimizeSingleArchitecture above

  private def halfSingleSramSizes(
                                   archResultBuffer: ArrayBuffer[ArchitectureResult],
                                 ): ArrayBuffer[ArchitectureResult] = {

    @tailrec
    def halfSingleSramSize(currentEval: ArchitectureResult): ArchitectureResult= {

      val currentArch  = currentEval.architecture
      val currentSingleSramSize = currentArch.arrayConfig.dataflow match {
        case Dataflow.Is =>
          currentArch.singleBufferLimitKbA
        case Dataflow.Os =>
          currentArch.singleBufferLimitKbA
        case Dataflow.Ws =>
          currentArch.singleBufferLimitKbB
      }

      if (currentArch.arrayConfig.dataflow == Dataflow.Os) {
        assert(currentArch.singleBufferLimitKbA == currentArch.singleBufferLimitKbB,
          s"For Output Stationary dataflow, SRAM A and SRAM B must have the same size. " +
            s"Found: SRAM A = ${currentArch.singleBufferLimitKbA} KB, SRAM B = ${currentArch.singleBufferLimitKbB} KB")
      }

      val currentSimulationResult = currentEval.simulationResult

      if(currentSingleSramSize <= minSramSize){
        return currentEval
      }

      val nextSramSize = currentSingleSramSize / 2

      val nextArchitecture = currentArch.arrayConfig.dataflow match {
        case Dataflow.Is =>
          currentArch.withSramBufferSize(nextSramSize, DataType.A)
        case Dataflow.Os =>
          currentArch
            .withSramBufferSize(nextSramSize, DataType.A)
            .withSramBufferSize(nextSramSize, DataType.B)
        case Dataflow.Ws =>
          currentArch.withSramBufferSize(nextSramSize, DataType.B)
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
    if (results.isEmpty) {
      println(s"Warning: No results to rank - returning empty buffer")
      log(s"\t\tWarning: No results to rank - returning empty buffer")
      return ArrayBuffer.empty[ArchitectureResult]
    }
    val metric = simConfig.metric

    val threshold = metric match {
      case SystemArchitectureOptimizerBo.OptimizationMetric.Cycle =>
        val minCycle: Long = results.map(_.simulationResult.cycle).min
        minCycle.toDouble * (1.0 + marginPercent/100.0)
      case SystemArchitectureOptimizerBo.OptimizationMetric.Energy =>
        val minEnergy = results.flatMap(_.simulationResult.energyPj).min
        minEnergy * (1.0 + marginPercent/100.0)
      case SystemArchitectureOptimizerBo.OptimizationMetric.Area =>
        val minArea = results.flatMap(_.simulationResult.areaUm2).min
        minArea * (1.0 + marginPercent/100.0)
      case SystemArchitectureOptimizerBo.OptimizationMetric.TOPS =>
        val minAreaEnergyProduct = results.flatMap(_.simulationResult.tops).max
        minAreaEnergyProduct * (1.0 - marginPercent/100.0)
    }

    val filteredResults = results.filter { result =>
      metric match {
        case SystemArchitectureOptimizerBo.OptimizationMetric.Cycle =>
          result.simulationResult.cycle <= threshold.toLong
        case SystemArchitectureOptimizerBo.OptimizationMetric.Energy =>
          result.simulationResult.energyPj.exists(_ <= threshold)
        case SystemArchitectureOptimizerBo.OptimizationMetric.Area =>
          result.simulationResult.areaUm2.exists(_ <= threshold)
        case SystemArchitectureOptimizerBo.OptimizationMetric.TOPS =>
          result.simulationResult.tops.exists(_ >= threshold)
      }
    }

    val sortedResults = metric match {
      case SystemArchitectureOptimizerBo.OptimizationMetric.Cycle =>
        filteredResults.sortBy(_.simulationResult.cycle)
      case SystemArchitectureOptimizerBo.OptimizationMetric.Energy =>
        filteredResults.sortBy(_.simulationResult.energyPj.get)
      case SystemArchitectureOptimizerBo.OptimizationMetric.Area =>
        filteredResults.sortBy(_.simulationResult.areaUm2.get)
      case SystemArchitectureOptimizerBo.OptimizationMetric.TOPS =>
        filteredResults.sortBy(_.simulationResult.tops.get).reverse
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

  private def buildSrams(
                          arrayConfig: ArrayConfig,
                          simConfig: SystemArchitectureOptimizerBo.SimulationConfig,
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

    val sramReferenceDataA: Option[SramReferenceData] = simConfig.sramReferenceDataVector.flatMap{ vector =>
      vector.find{ data =>
        data.capacityKb == singleBufferLimitKbA && data.bandwidthBits >= arrayConfig.bandwidthOfInputA
      }
    }

    val sramReferenceDataB: Option[SramReferenceData] = simConfig.sramReferenceDataVector.flatMap{ vector =>
      vector.find{ data =>
        data.capacityKb == singleBufferLimitKbB && data.bandwidthBits >= arrayConfig.bandwidthOfInputB
      }
    }

    val sramReferenceDataC: Option[SramReferenceData] = simConfig.sramReferenceDataVector.flatMap{ vector =>
      vector.find{ data =>
        data.capacityKb == singleBufferLimitKbC && data.bandwidthBits >= arrayConfig.outputBandwidth
      }
    }


    if(!(capacityA > 0 && capacityB > 0 && capacityC > 0 )) {


      Left(SramBuildError("SRAM Cannot contain even 1 tile"))
    } else if (sramReferenceDataA.isEmpty || sramReferenceDataB.isEmpty || sramReferenceDataC.isEmpty){

      Left(SramBuildError("Cannot find SRAM data from external reports"))

    } else {


      val sramA = new DoubleBufferSram(
        dataType = DataType.A,
        outputBandwidth = arrayConfig.bandwidthOfInputA,
        singleBufferTileCapacity = capacityA,
        singleBufferLimitKb = singleBufferLimitKbA,
        referenceData = sramReferenceDataA,
        loggerOption = loggerOption
      )
      val sramB = new DoubleBufferSram(
        dataType = DataType.B,
        outputBandwidth = arrayConfig.bandwidthOfInputB,
        singleBufferTileCapacity = capacityB,
        singleBufferLimitKb = singleBufferLimitKbB,
        referenceData = sramReferenceDataB,
        loggerOption = loggerOption
      )
      val sramC = new OutputDoubleBufferSram(
        outputBandwidth = simConfig.offChipMemoryBandwidth,
        singleBufferTileCapacity = capacityC,
        singleBufferLimitKb = singleBufferLimitKbC,
        referenceData = sramReferenceDataC,
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

        sramReadAccessCountC = simulation.getSramReadAccessCountC,
        sramWriteAccessCountC = simulation.getSramWriteAccessCountC,

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

        sramReadEnergyPjB = simulation.getSramReadEnergyB,
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

        tops = simulation.calculateTOPS

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
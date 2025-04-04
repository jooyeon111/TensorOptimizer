package simulation

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import common.Dataflow

class ArchitectureEvaluator(
  val simConfig: SystemArchitectureOptimizer.SimulationConfig,
  val architectureCandidates: ArrayBuffer[Architecture],
  val marginPercent: Double =  5.0,
  val minSramSize: Int,
  val loggerOption: LoggerOption,
) extends Logger {

  setMode(loggerOption)

  private val executableCandidates = ArrayBuffer.empty[Architecture]
  private val calculatedAllResults = ArrayBuffer.empty[ArchitectureResult]
  private val rankedCalculatedResults = ArrayBuffer.empty[ArchitectureResult]
  private val archOptimizedResults = ArrayBuffer.empty[ArchitectureResult]
  private val rankedArchOptimizedResults = ArrayBuffer.empty[ArchitectureResult]
  private val singleSramOptimizedResults = ArrayBuffer.empty[ArchitectureResult]
  private val rankedSingleSramOptimizedResults = ArrayBuffer.empty[ArchitectureResult]

  def run(): Unit = {

    //validate
    executableCandidates ++= validateCandidates(architectureCandidates)
    logExecutableArch(
      candidates = architectureCandidates,
      validCandidates = executableCandidates,
    )

    //simulate all configs
    calculatedAllResults ++= process1(executableCandidates)
    rankedCalculatedResults ++= rankResults(calculatedAllResults)
    logSimulatedArch(
      validCandidates = executableCandidates,
      simulatedCandidates = calculatedAllResults,
      rankedSimulatedCandidates = rankedCalculatedResults,
    )

    //optimize arch
    archOptimizedResults ++= process2(rankedCalculatedResults)
    rankedArchOptimizedResults ++= rankResults(archOptimizedResults)
    logProcess(
      preProcess = rankedCalculatedResults,
      postProcess = archOptimizedResults,
      rankedPostProcess = rankedArchOptimizedResults
    )

    //optimize single sram
    singleSramOptimizedResults ++= process3(rankedArchOptimizedResults)
    rankedSingleSramOptimizedResults ++= rankResults(singleSramOptimizedResults)
    logProcess(
      preProcess = rankedArchOptimizedResults,
      postProcess = rankedSingleSramOptimizedResults,
      rankedPostProcess = rankedSingleSramOptimizedResults
    )

    log(s"[Evaluation Complete]")

  }

  def showTopK(k: Int): Unit = {
    singleSramOptimizedResults.take(k).foreach(logSummary)
  }

  private def validateCandidates(archBuffer: ArrayBuffer[Architecture]): ArrayBuffer[Architecture] = {
    log("\t[Check Initial Streaming Dimension]")
    println("Check Initial Streaming Dimension")
    checkInitialStreamingDimensions(archBuffer)
  }

  private def process1(archBuffer: ArrayBuffer[Architecture]): ArrayBuffer[ArchitectureResult] = {
    log("\t[process1: Find High Efficiency STA configs]")
    println("process1: Find High Efficiency STA configs")
    evaluateAllArchitectures(archBuffer)
  }

  private def process2(archResultBuffer: ArrayBuffer[ArchitectureResult]): ArrayBuffer[ArchitectureResult] = {
    log(s"\t[process2: Optimize Architecture]")
    println("process2: Optimize Architecture")
    optimizeSramStreamingTradeOffs(archResultBuffer)
  }

  private def process3(archResultBuffer: ArrayBuffer[ArchitectureResult]): ArrayBuffer[ArchitectureResult] = {
    log(s"\t[process3: Optimize Single SRAM]")
    println("process3: Optimize Single SRAM")
    halfSingleSramSizes(archResultBuffer)
  }

  private def logExecutableArch(
    candidates: ArrayBuffer[Architecture],
    validCandidates: ArrayBuffer[Architecture]
  ): Unit = {
    log(s"\t\t${candidates.size} architectures were candidates")
    log(s"\t\t${validCandidates.size} architectures were valid")

  }

  private def logSimulatedArch(
    validCandidates: ArrayBuffer[Architecture],
    simulatedCandidates: ArrayBuffer[ArchitectureResult],
    rankedSimulatedCandidates: ArrayBuffer[ArchitectureResult],
  ): Unit = {
    log("")
    log(s"\t\t${validCandidates.size} valid architectures were passed to process")
    log(s"\t\t${simulatedCandidates.size} architectures were successfully processed")
    log(s"\t\t${rankedSimulatedCandidates.size} architectures meet performance criteria")

    // Summary statistics
    if (simulatedCandidates.nonEmpty) {
      val cycleTimes = simulatedCandidates.map(_.simulationResult.cycle)
      val minCycle = cycleTimes.min
      val maxCycle = cycleTimes.max
      val avgCycle = cycleTimes.sum / cycleTimes.length

      log("")
      log("\t\t[Performance Statistics]")
      log(s"\t\t\tMinimum cycle time: $minCycle")
      log(s"\t\t\tMaximum cycle time: $maxCycle")
      log(s"\t\t\tAverage cycle time: $avgCycle")

      // Show energy and area metrics if available
      if (simulatedCandidates.exists(_.simulationResult.energyPj.isDefined)) {
        val energyValues = simulatedCandidates.flatMap(_.simulationResult.energyPj)
        if (energyValues.nonEmpty) {
          val minEnergy = energyValues.min
          val maxEnergy = energyValues.max
          val avgEnergy = energyValues.sum / energyValues.length

          log(s"\t\t\tMinimum energy: ${String.format("%.2f", minEnergy)} pJ")
          log(s"\t\t\tMaximum energy: ${String.format("%.2f", maxEnergy)} pJ")
          log(s"\t\t\tAverage energy: ${String.format("%.2f", avgEnergy)} pJ")
        }
      }

      if (simulatedCandidates.exists(_.simulationResult.areaUm2.isDefined)) {
        val areaValues = simulatedCandidates.flatMap(_.simulationResult.areaUm2)
        if (areaValues.nonEmpty) {
          val minArea = areaValues.min
          val maxArea = areaValues.max
          val avgArea = areaValues.sum / areaValues.length

          log(s"\t\t\tMinimum area: ${String.format("%.2f", minArea)} um²")
          log(s"\t\t\tMaximum area: ${String.format("%.2f", maxArea)} um²")
          log(s"\t\t\tAverage area: ${String.format("%.2f", avgArea)} um²")
        }
      }
    }

    if (rankedSimulatedCandidates.nonEmpty) {
      log("")
      log(s"\t[Top ${Math.min(5, rankedSimulatedCandidates.size)} Ranked Architectures]")
      rankedSimulatedCandidates.take(5).zipWithIndex.foreach { case (result, idx) =>
        log(s"\t\t[Rank ${idx + 1}]")
        logSummary(result)
      }
    }

    log("")
    log("\tNow proceeding to optimization phase...")
    log("")

  }


  private def logProcess(
    preProcess: ArrayBuffer[ArchitectureResult],
    postProcess: ArrayBuffer[ArchitectureResult],
    rankedPostProcess: ArrayBuffer[ArchitectureResult],
  ): Unit = {

    log(s"\t${preProcess.size} architectures were processed")
    log(s"\t${postProcess.size} optimized architectures were evaluated")
    log(s"\t${rankedPostProcess.size} architectures meet performance criteria")

    // Calculate improvement statistics
    if (preProcess.nonEmpty && rankedPostProcess.nonEmpty) {
      val preProcessBest = preProcess.head
      val postProcessBest = rankedPostProcess.head

      log("")
      log("\t\t[Performance Improvements]")

      // Cycle improvement
      val cycleImprovement = (preProcessBest.simulationResult.cycle - postProcessBest.simulationResult.cycle).toDouble /
        preProcessBest.simulationResult.cycle * 100
      log(s"\t\t\tCycle time: ${String.format("%.2f", cycleImprovement)}% improvement")

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

      // Resource usage changes
      log("")
      log("\t\t[Resource Utilization Changes]")
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
    }

    // Top 5 architectures after optimization
    if (rankedPostProcess.nonEmpty) {
      log("")
      log(s"\t[Top ${Math.min(5, rankedPostProcess.size)} Optimized Architectures]")
      rankedPostProcess.take(5).zipWithIndex.foreach { case (result, idx) =>
        log(s"\t\t[Rank ${idx + 1}]")
        logSummary(result)
      }
    }

    log("")

  }


  private def logSummary(ArchitectureResult: ArchitectureResult): Unit = {
    val simulationResult = ArchitectureResult.simulationResult
    val architecture = ArchitectureResult.architecture

    if(simulationResult.isEnergyReportValid && simulationResult.isAreaReportValid){
      log(s"\t\t\t[${architecture.arrayConfig.arrayConfigString}]")
      log(s"\t\t\t\tCycle: ${simulationResult.cycle}")
      log(s"\t\t\t\tArea: ${String.format("%.2f", simulationResult.areaUm2.get)} mm^2")
      log(s"\t\t\t\tEnergy: ${String.format("%.2f", simulationResult.energyPj.get)} pJ")
      log(s"\t\t\t\tStreaming Dimension Size: ${architecture.streamingDimensionSize}")
      log(s"\t\t\t\tSingleBuffer A: ${architecture.singleBufferLimitKbA} KB")
      log(s"\t\t\t\tSingleBuffer B: ${architecture.singleBufferLimitKbB} KB")
      log(s"\t\t\t\tSingleBuffer C: ${architecture.singleBufferLimitKbC} KB\n")
    } else {
      log(s"\t\t\t[${architecture.arrayConfig.arrayConfigString}]")
      log(s"\t\t\t\tCycle: ${simulationResult.cycle},")
      log(s"\t\t\t\tStreaming Dimension Size: ${architecture.streamingDimensionSize}")
      log(s"\t\t\t\tSingleBuffer A: ${architecture.singleBufferLimitKbA} KB")
      log(s"\t\t\t\tSingleBuffer B: ${architecture.singleBufferLimitKbB} KB")
      log(s"\t\t\t\tSingleBuffer C: ${architecture.singleBufferLimitKbC} KB\n")
    }

  }
  private def checkInitialStreamingDimensions(archBuffer: ArrayBuffer[Architecture]): ArrayBuffer[Architecture] = {

    val executableArchBuffer = ArrayBuffer.empty[Architecture]

    archBuffer.foreach { arch =>
      var currentArch = arch
      var success = false
      var attempts = 0
      val maxAttempts = 5

      while (!success && attempts < maxAttempts) {

        val layer = new Layer(
          layerName = simConfig.layerName,
          gemmDimension = simConfig.layerGemmDimension,
          arrayConfig = arch.arrayConfig,
          streamingDimensionSize = arch.streamingDimensionSize,
          dramUploadOrder = arch.dramUploadOrder,
          loggerOption = loggerOption
        )

        val tileSizeA = layer.operationVector.head.generateTileA.dims.memorySize
        val tileSizeB = layer.operationVector.head.generateTileB.dims.memorySize
        val tileSizeC = layer.operationVector.head.generateTileC.dims.memorySize

        buildSrams(
          arrayConfig = arch.arrayConfig,
          simConfig = simConfig,
          singleBufferLimitKbA = arch.singleBufferLimitKbA,
          singleBufferLimitKbB = arch.singleBufferLimitKbB,
          singleBufferLimitKbC = arch.singleBufferLimitKbC,
          tileSizeA = tileSizeA,
          tileSizeB = tileSizeB,
          tileSizeC = tileSizeC,
          isFirstTrial = attempts == 0,
          loggerOption = loggerOption
        ) match {
          case Right(_) =>
            success = true
            executableArchBuffer += currentArch

          case Left(_) =>
            if (currentArch.streamingDimensionSize > 1) {

              log(s"\t\tStreaming Dimension ${currentArch.streamingDimensionSize} is too high to build SRAM")
              val newStreamingDimSize = Math.max(1, currentArch.streamingDimensionSize / 2)

              log(s"\t\thalf the streaming dimension as $newStreamingDimSize")
              currentArch = currentArch.withStreamingDimensionSize(newStreamingDimSize)

              attempts += 1

            } else {
              attempts = maxAttempts
            }
        }
      }

      if (!success) {

        log(s"\t\tFailed to build arch ${arch.arrayConfig.arrayConfigString}")
        log(s"\t\tInitial streaming dimension is too high change max attempts in checkInitialStreamingDimensions")
        log("")
      }

    }

    log(s"")

    executableArchBuffer

  }

  private def evaluateAllArchitectures(architectureBuffer: ArrayBuffer[Architecture]): ArrayBuffer[ArchitectureResult] = {
    architectureBuffer.map { arch =>
      buildAndRunSimulation(architecture = arch)
    }
  }

  private def optimizeSramStreamingTradeOffs(
    archResultBuffer: ArrayBuffer[ArchitectureResult],
  ): ArrayBuffer[ArchitectureResult] = {
    archResultBuffer.map(arch => optimizeSramStreamingTradeOff(arch))
  }

  private def optimizeSramStreamingTradeOff(
    archResult: ArchitectureResult
  ): ArchitectureResult = {

    def optimizeRecursively(
      currentEval: ArchitectureResult,
      maxIterations: Int = 10,
      iteration: Int = 0
    ): ArchitectureResult = {
      if (iteration >= maxIterations) {
        log(s"\t\t\tReached iteration limit (${iteration}). Stopping optimization.")
        return currentEval
      }

      val currentArch = currentEval.architecture
      val currentResult = currentEval.simulationResult

      log(s"\t\t\t[Iteration ${iteration}] Current config: SRAM=${currentArch.singleBufferLimitKbA}KB, " +
        s"Streaming=${currentArch.streamingDimensionSize}")

      if (currentArch.singleBufferLimitKbA <= minSramSize) {
        log(s"\t\t\tCannot reduce SRAM size further (minimum reached: ${minSramSize}KB)")
        return currentEval
      }

      // STEP 1: Try halving ONLY the SRAM size first
      val nextSramSize = math.max(minSramSize, currentArch.singleBufferLimitKbA / 2)
      val sramReducedArch = currentArch.withUniformSramSizes(nextSramSize)

      log(s"\t\t\tTrying to reduce SRAM size from ${currentArch.singleBufferLimitKbA}KB to ${nextSramSize}KB")

      val sramReducedArchResult = buildAndRunSimulation(sramReducedArch)
      val isSramReductionValid = sramReducedArchResult.simulationResult.cycle != Long.MaxValue

      if (isSramReductionValid) {
        val isImproved = simConfig.metric match {
          case SystemArchitectureOptimizer.OptimizationMetric.Cycle =>
            sramReducedArchResult.simulationResult.cycle < currentResult.cycle
          case SystemArchitectureOptimizer.OptimizationMetric.Energy =>
            sramReducedArchResult.simulationResult.energyPj.exists(_ < currentResult.energyPj.get )
          case SystemArchitectureOptimizer.OptimizationMetric.Area =>
            sramReducedArchResult.simulationResult.areaUm2.exists(_ <= currentResult.areaUm2.get )
        }

        if (isImproved) {
          log(s"\t\t\tSRAM reduction successful! Continuing optimization with reduced SRAM.")
          // If SRAM reduction was successful, continue optimization with the reduced SRAM
          return optimizeRecursively(sramReducedArchResult, maxIterations, iteration + 1)
        }
      } else {
        log(s"\t\t\tSRAM reduction failed to build. Trying combined reduction next.")
      }


      // If SRAM reduction was successful, return that result
      if (currentArch.streamingDimensionSize <= 1) {
        log(s"\t\t\tCannot reduce streaming dimension further (minimum reached: 1")
        return currentEval
      }

      val nextStreamingSize = math.max(1, currentArch.streamingDimensionSize / 2)
      val combinedReducedArch = currentArch
        .withUniformSramSizes(nextSramSize)
        .withStreamingDimensionSize(nextStreamingSize)

      log(s"\t\t\tTrying combined reduction: SRAM=${nextSramSize}KB, Streaming=${nextStreamingSize}")

      val combinedReductionResult = buildAndRunSimulation(combinedReducedArch)
      val isCombinedResultValid = combinedReductionResult.simulationResult.cycle != Long.MaxValue

      if(isCombinedResultValid){
        val isImproved = simConfig.metric match {
          case SystemArchitectureOptimizer.OptimizationMetric.Cycle =>
            combinedReductionResult.simulationResult.cycle <= currentResult.cycle
          case SystemArchitectureOptimizer.OptimizationMetric.Energy =>
            combinedReductionResult.simulationResult.energyPj.exists(_ <= currentResult.energyPj.get )
          case SystemArchitectureOptimizer.OptimizationMetric.Area =>
            combinedReductionResult.simulationResult.areaUm2.exists(_ <= currentResult.areaUm2.get )
        }

        if(isImproved){
          log(s"\t\t\tCombined reduction successful! Continuing optimization.")
          return optimizeRecursively(combinedReductionResult, maxIterations, iteration + 1)

        } else {
          log(s"\t\t\tCombined reduction worsened performance. Stopping optimization.")
        }

      } else {
        log(s"\t\t\tCombined reduction failed to build. Stopping optimization.")
      }

      // Return the result of combined reduction or the current evaluation if it failed

      currentEval

    }

    // Start the recursive optimization process
    log(s"\t\tStarting optimization for ${archResult.architecture.arrayConfig.arrayConfigString}")
    val result = optimizeRecursively(archResult)
    log(s"\t\t\tOptimization complete. Final config: SRAM=${result.architecture.singleBufferLimitKbA}KB, " +
      s"Streaming=${result.architecture.streamingDimensionSize}")
    log(s"")
    result

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
        log(s"\t\t\t${currentArch.arrayConfig.arrayConfigString} cannot reduce SRAM size further (minimum reached: ${minSramSize}KB)")
        return currentEval
      }

      val nextSramSize = currentSingleSramSize / 2

      val nextArchitecture = currentArch.arrayConfig.dataflow match {
        case Dataflow.Is => currentArch.withSramBufferSize(nextSramSize, DataType.A)
        case Dataflow.Os => currentArch.withSramBufferSize(nextSramSize, DataType.C)
        case Dataflow.Ws => currentArch.withSramBufferSize(nextSramSize, DataType.B)
      }

      val newEval = buildAndRunSimulation(nextArchitecture)

      simConfig.metric match {
        case SystemArchitectureOptimizer.OptimizationMetric.Cycle =>
          if (newEval.simulationResult.cycle < currentSimulationResult.cycle)
            halfSingleSramSize(newEval)
          else
            currentEval

        case SystemArchitectureOptimizer.OptimizationMetric.Energy =>
          if (newEval.simulationResult.energyPj.get < currentSimulationResult.energyPj.get)
            halfSingleSramSize(newEval)
          else
            currentEval

        case SystemArchitectureOptimizer.OptimizationMetric.Area =>
          if (newEval.simulationResult.areaUm2.get <=currentSimulationResult.areaUm2.get)
            halfSingleSramSize(newEval)
          else
            currentEval
      }

    }

    archResultBuffer.map(result =>halfSingleSramSize(result))

  }

  private def rankResults(
    results: ArrayBuffer[ArchitectureResult],
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
    }

    val filteredResults = results.filter { result =>
      metric match {
        case SystemArchitectureOptimizer.OptimizationMetric.Cycle =>
          result.simulationResult.cycle <= threshold
        case SystemArchitectureOptimizer.OptimizationMetric.Energy =>
          result.simulationResult.energyPj.exists(_ <= threshold)
        case SystemArchitectureOptimizer.OptimizationMetric.Area =>
          result.simulationResult.areaUm2.exists(_ <= threshold)
      }
    }

    val sortedResults = metric match {
      case SystemArchitectureOptimizer.OptimizationMetric.Cycle =>
        filteredResults.sortBy(_.simulationResult.cycle)
      case SystemArchitectureOptimizer.OptimizationMetric.Energy =>
        filteredResults.sortBy(_.simulationResult.energyPj.get)
      case SystemArchitectureOptimizer.OptimizationMetric.Area =>
        filteredResults.sortBy(_.simulationResult.areaUm2.get)
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
    simConfig: SystemArchitectureOptimizer.SimulationConfig,
    singleBufferLimitKbA: Int,
    singleBufferLimitKbB: Int,
    singleBufferLimitKbC: Int,
    tileSizeA: Int,
    tileSizeB: Int,
    tileSizeC: Int,
    isFirstTrial: Boolean = true,
    loggerOption: LoggerOption
  ): Either[SramBuildError,(DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram)] = {

    val capacityA = singleBufferLimitKbA * 8 * 1024 / tileSizeA
    val capacityB = singleBufferLimitKbB * 8 * 1024 / tileSizeB
    val capacityC = singleBufferLimitKbC * 8 * 1024 / tileSizeC

    if(!(capacityA > 0 && capacityB > 0 && capacityC > 0 )) {
      if(isFirstTrial) {
        log(s"\t\tBuilding SRAM is failed: ${arrayConfig.arrayConfigString}")
      }
      Left(SramBuildError("SRAM Cannot contain even 1 tile"))
    } else {

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
        outputBandwidth = simConfig.dramBandwidth,
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
  ): Either[SramBuildError,(Layer, Dram, Array, (DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram), Interface)] = {

    val layer = new Layer(
      layerName = simConfig.layerName,
      gemmDimension = simConfig.layerGemmDimension,
      arrayConfig = architecture.arrayConfig,
      streamingDimensionSize = architecture.streamingDimensionSize,
      dramUploadOrder = architecture.dramUploadOrder,
      loggerOption = loggerOption
    )

    val dram = new Dram(
      outputBandwidth = simConfig.dramBandwidth,
      referenceData = simConfig.dramReferenceData,
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
          dram = dram,
          sramA = sramA,
          sramB = sramB,
          sramC = sramC,
          array = array
        )
        Right((layer, dram, array, srams, interface))

      case Left(error) =>
        Left(error)
    }

  }


  private def runSimulation(
    components: (Layer, Dram, Array, (DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram), Interface)
  ): SimulationResult = {

    val (layer, dram, array, srams, interface)= components
    val (sramA, sramB, sramC) = srams
    
    val simulation = new SystemSimulator(
      dram = dram,
      sramA = sramA,
      sramB = sramB,
      sramC = sramC,
      interface = interface,
      layer = layer,
      array = array,
      loggerOption = loggerOption,
    )

    try {
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

        dramReadAccessCount = simulation.getDramReadAccessCount,
        dramWriteAccessCount = simulation.getDramWriteAccessCount,
        sramReadAccessCountA = simulation.getSramReadAccessCountA,
        sramWriteAccessCountA = simulation.getSramWriteAccessCountA,
        sramReadAccessCountB = simulation.getSramReadAccessCountB,
        sramWriteAccessCountB = simulation.getSramWriteAccessCountB,

        dramHitRatio = simulation.getTotalDramHitCount,
        dramMissRatio = simulation.getTotalDramMissCount,

        sramHitRatioA = simulation.getSramHitRatioA,
        sramHitRatioB = simulation.getSramHitRatioB,
        sramMissRatioA = simulation.getSramMissRatioA,
        sramMissRatioB = simulation.getSramMissRatioB,
        sramHitRatio = simulation.getTotalSramHitRatio,
        sramMissRatio = simulation.getTotalSramMissRatio,

        dramStallCount = simulation.getDramStallCount,

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

        dramReferenceData = simulation.getDramRefData,
        sramReferenceDataTable = simulation.getSramRefDataTable,
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

        dramReadEnergyPj = simulation.getDramReadEnergy,
        dramWriteEnergyPj = simulation.getDramWriteEnergy,
        dramEnergyPj = simulation.getDramEnergy,

        arrayDynamicEnergyPj = simulation.getArrayDynamicEnergy,
        arrayLeakageEnergyPj = simulation.getArrayLeakageEnergy,
        arrayEnergy = simulation.getArrayEnergy,

        energyPj = simulation.getTotalEnergy,

        sramAreaUm2A = simulation.getSramAreaA,
        sramAreaUm2B = simulation.getSramAreaB,
        sramAreaUm2C = simulation.getSramAreaC,

        arrayAreaUm2 = simulation.getArrayArea,
        areaUm2 = simulation.getArea

      )


    } catch {
      case _: RunTimeError =>
        Console.err.println("Program Wrong")
        sys.exit(1)
    }

  }

}

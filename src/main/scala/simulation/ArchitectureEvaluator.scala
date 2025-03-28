package simulation

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import common.Dataflow

class ArchitectureEvaluator(
  val simConfig: SystemArchitectureOptimizer.SimulationConfig,
  val architectureCandidates: ArrayBuffer[Architecture],
  val marginPercent: Double =  10.0,
  val loggerOption: LoggerOption,
) extends Logger {

  setMode(loggerOption)

  private val firstProcessResults = ArrayBuffer.empty[Architecture]
  private val calculatedAllConfigs = ArrayBuffer.empty[ArchitectureEvaluation]
  private val secondProcessResults = ArrayBuffer.empty[ArchitectureEvaluation]
  private val thirdProcessResults = ArrayBuffer.empty[ArchitectureEvaluation]
  private val fourthProceeResults = ArrayBuffer.empty[ArchitectureEvaluation]
  private val finalProcessResults = ArrayBuffer.empty[ArchitectureEvaluation]

  def run(): Unit = {
    //PHASE1: Check initial streaming dimension sizes
    log("[Evaluation START]")
    log("\t\t[PHASE1]")
    checkInitialStreamingDimensions()
    log(s"\t\tChecking initial condition of candidates ${firstProcessResults.size} has survived" +
      s" out of ${architectureCandidates.size}")
    log("")

    log("\t\t[PHASE2]")
    log(s"\t\tFinding high efficiency configurations")
    evaluateAllArchitectures()
    secondProcessResults ++= filterArchitectureByMetric(calculatedAllConfigs, simConfig.metric)
    secondProcessResults.foreach(logSummary)
    log(s"")

    log(s"\t\t[PHASE 3]")
    thirdProcessResults ++= optimizeStreamingSizes(secondProcessResults)
    thirdProcessResults.foreach(logSummary)

    log(s"")
    log(s"\t\t[PHASE 4]")
    fourthProceeResults ++= optimizeSramSizes(thirdProcessResults)
    fourthProceeResults.foreach(logSummary)

    log(s"")

    log(s"\t\t[PHASE 5]")
    finalProcessResults ++= optimizeSingleSramSizes(fourthProceeResults)
    finalProcessResults.foreach(logSummary)

    log(s"")

  }


  private def logSummary(architectureEvaluation: ArchitectureEvaluation): Unit = {
    val simulationResult = architectureEvaluation.simulationResult
    val architecture = architectureEvaluation.architecture

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

  private def checkInitialStreamingDimensions(): Unit = {

    for( arch <- architectureCandidates ){
      var currentArch = arch
      var success = false
      var attempts = 0
      val maxAttempts = 5


      while(!success && attempts < maxAttempts){
        try {

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
            loggerOption = loggerOption
          )

          success = true
          firstProcessResults += currentArch

        } catch {
          case _: Exception =>
            if(currentArch.streamingDimensionSize > 1) {
              val newStreamingDimSize = Math.max(1, currentArch.streamingDimensionSize / 2)
              currentArch = currentArch.withStreamingDimensionSize(newStreamingDimSize)
            } else {
              attempts = maxAttempts
            }
        }

        attempts += 1
      }

      if(!success){
        println("Failed to build arch")
      }

    }

  }

  private def evaluateAllArchitectures(): Unit = {
    architectureCandidates.foreach { arch =>
      calculatedAllConfigs += buildAndRunSimulation(simConfig = simConfig, architecture = arch)
    }
  }


  private def optimizeStreamingSizes(
    evaluationBuffer: ArrayBuffer[ArchitectureEvaluation],
  ): ArrayBuffer[ArchitectureEvaluation] = {


    def optimizeStreamingDimensionSize(result: ArchitectureEvaluation): ArchitectureEvaluation= {

      val currentArchitecture = result.architecture
      val currentStreamingDimensionSize = currentArchitecture.streamingDimensionSize
      val currentSimulationResult = result.simulationResult

      val nextStreamingDimensionSize = currentStreamingDimensionSize / 2
      val nextArchitecture = currentArchitecture.withStreamingDimensionSize(nextStreamingDimensionSize)
      val newResult = buildAndRunSimulation(simConfig, nextArchitecture)

      simConfig.metric match {
        case SystemArchitectureOptimizer.OptimizationMetric.Cycle =>
          if (newResult.simulationResult.cycle < currentSimulationResult.cycle)
            newResult
          else
            result

        case SystemArchitectureOptimizer.OptimizationMetric.Energy =>
          if (newResult.simulationResult.energyPj.get < currentSimulationResult.energyPj.get)
            newResult
          else
            result

        case SystemArchitectureOptimizer.OptimizationMetric.Area =>
          if (newResult.simulationResult.areaUm2.get < currentSimulationResult.areaUm2.get)
            newResult
          else
            result
      }

    }

    val result = evaluationBuffer.map(result =>optimizeStreamingDimensionSize(result))

    filterArchitectureByMetric(result, simConfig.metric)

  }


  private def optimizeSramSizes(
    evaluationBuffer: ArrayBuffer[ArchitectureEvaluation],
  ): ArrayBuffer[ArchitectureEvaluation] = {


    def optimizeSramSize(result: ArchitectureEvaluation): ArchitectureEvaluation= {

      val currentArchitecture = result.architecture
      val currentSramSize = currentArchitecture.singleBufferLimitKbA
      assert(currentArchitecture.isAllSramSameSize, "[error] SRAM Size different in optimizng sramSize")
      val currentSimulationResult = result.simulationResult

      val nextSramSize = currentSramSize / 2
      val nextArchitecture = currentArchitecture.withUniformSramSizes(nextSramSize)
      val newResult = buildAndRunSimulation(simConfig, nextArchitecture)

      simConfig.metric match {
        case SystemArchitectureOptimizer.OptimizationMetric.Cycle =>
          if (newResult.simulationResult.cycle < currentSimulationResult.cycle)
            newResult
          else
            result

        case SystemArchitectureOptimizer.OptimizationMetric.Energy =>
          if (newResult.simulationResult.energyPj.get < currentSimulationResult.energyPj.get)
            newResult
          else
            result

        case SystemArchitectureOptimizer.OptimizationMetric.Area =>
          if (newResult.simulationResult.areaUm2.get < currentSimulationResult.areaUm2.get)
            newResult
          else
            result
      }

    }

    val result = evaluationBuffer.map(result =>optimizeSramSize(result))

    filterArchitectureByMetric(result, simConfig.metric)

  }

  private def optimizeSingleSramSizes(
    evaluationBuffer: ArrayBuffer[ArchitectureEvaluation],
  ): ArrayBuffer[ArchitectureEvaluation] = {


    @tailrec
    def optimizeSingleSramSize(result: ArchitectureEvaluation): ArchitectureEvaluation= {

      val currentArchitecture = result.architecture
      val currentSingleSramSize = currentArchitecture.arrayConfig.dataflow match {
        case Dataflow.Is => currentArchitecture.singleBufferLimitKbA
        case Dataflow.Os => currentArchitecture.singleBufferLimitKbC
        case Dataflow.Ws => currentArchitecture.singleBufferLimitKbB
      }

      val currentSimulationResult = result.simulationResult

      val nextSramSize = currentSingleSramSize / 2
      val nextArchitecture = currentArchitecture.arrayConfig.dataflow match {
        case Dataflow.Is => currentArchitecture.withSramBufferSize(nextSramSize, DataType.A)
        case Dataflow.Os => currentArchitecture.withSramBufferSize(nextSramSize, DataType.C)
        case Dataflow.Ws => currentArchitecture.withSramBufferSize(nextSramSize, DataType.B)
      }

      val newResult = buildAndRunSimulation(simConfig, nextArchitecture)

      simConfig.metric match {
        case SystemArchitectureOptimizer.OptimizationMetric.Cycle =>
          if (newResult.simulationResult.cycle < currentSimulationResult.cycle)
            optimizeSingleSramSize(newResult)
          else
            result

        case SystemArchitectureOptimizer.OptimizationMetric.Energy =>
          if (newResult.simulationResult.energyPj.get < currentSimulationResult.energyPj.get)
            optimizeSingleSramSize(newResult)
          else
            result

        case SystemArchitectureOptimizer.OptimizationMetric.Area =>
          if (newResult.simulationResult.areaUm2.get <=currentSimulationResult.areaUm2.get)
            optimizeSingleSramSize(newResult)
          else
            result
      }

    }

    val result = evaluationBuffer.map(result =>optimizeSingleSramSize(result))

    filterArchitectureByMetric(result, simConfig.metric)

  }

  private def filterArchitectureByMetric(
    results: ArrayBuffer[ArchitectureEvaluation],
    metric: SystemArchitectureOptimizer.OptimizationMetric.Value,
  ): ArrayBuffer[ArchitectureEvaluation] = {

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
    simConfig: SystemArchitectureOptimizer.SimulationConfig,
    architecture: Architecture
  ): ArchitectureEvaluation = {
    buildSimulationComp(simConfig, architecture) match {
      case Right(comp) =>
        val result = runSimulation(comp)
        ArchitectureEvaluation(architecture = architecture, simulationResult = result)
      case Left(_) =>
        ArchitectureEvaluation(architecture = architecture, simulationResult = SimulationResult(Long.MaxValue, Double.MaxValue, Double.MaxValue))
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
    loggerOption: LoggerOption
  ): Either[CompBuildError,(DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram)] = {

    val capacityA = singleBufferLimitKbA * 8 * 1024 / tileSizeA
    val capacityB = singleBufferLimitKbB * 8 * 1024 / tileSizeB
    val capacityC = singleBufferLimitKbC * 8 * 1024 / tileSizeC

    if(!(capacityA > 0 && capacityB > 0 && capacityC > 0 )) {
      println(s"Building SRAM is failed: ${arrayConfig.arrayConfigString}")
      Left(CompBuildError("SRAM Cannot contain even 1 tile increase SRAM size"))
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
    simConfig: SystemArchitectureOptimizer.SimulationConfig,
    architecture: Architecture
  ): Either[CompBuildError,(Layer, Dram, Array, (DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram), Interface)] = {

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
        println(s"Failed to build hardware components array configuration: ${architecture.arrayConfig.arrayConfigString}")
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

//  def returnTopK(k: Int): ArrayBuffer[ArchitectureEvaluation] = {
//    evaluatedArchitectures.take(k)
//  }

}

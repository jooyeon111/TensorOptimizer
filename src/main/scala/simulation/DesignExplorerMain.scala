package simulation


import common.{OutputPortCalculator, Dataflow}

import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import common.FilePaths
import scala.util.{Failure, Success, Try}

object DesignExplorerMain extends App with Logger with StreamingDimensionCalculator with OutputPortCalculator {

  object OptimizationMetric extends Enumeration{
    type OptimizationMetric = Value
    val Cycle, Energy, Area = Value
  }

  case class SimulationConfig(
    layerName: String,
    layerGemmDimension: GemmDimension,
    metric: OptimizationMetric.Value,
    bitWidthPortA: Int,
    bitWidthPortB: Int,
    bitWidthPortC: Int,
    dramBandwidth: Int,
    totalNumberOfMultipliers: Int,
    dramReferenceData: Option[DramReferenceData],
    sramReferenceDataVector: Option[Vector[SramReferenceData]],
    arrayReferenceData: Option[ArrayReferenceData],
  ) {
    def validate: Boolean = {
      val baseValidation = layerGemmDimension.validate &&
        totalNumberOfMultipliers > 0 &&
        bitWidthPortA > 0 &&
        bitWidthPortB > 0 &&
        bitWidthPortC > 0 &&
        dramBandwidth > 0 &&
        layerGemmDimension.validate

      val energyValidation = (dramReferenceData, sramReferenceDataVector, arrayReferenceData) match {
        case (Some(dram), Some(sram), Some(array)) =>
          dram.validate && sram.forall(_.validate) && array.validate
        case (None, None, None) =>
          true
        case _ =>
          false
      }

      baseValidation && energyValidation

    }
  }

  case class SingleBufferLimitKbs(
    limitA: Int,
    limitB: Int,
    limitC: Int,
  ) {
    val limitBitA: Int = limitA * 8 * 1024
    val limitBitB: Int = limitB * 8 * 1024
    val limitBitC: Int = limitC * 8 * 1024

    def isAllSramSizeSame: Boolean = (limitA == limitB) && (limitB == limitC)

  }

  case class Architecture(
    arrayConfig: ArrayConfig,
    streamingDimensionSize: Int,
    singleBufferLimitKbs: SingleBufferLimitKbs,
  ) {

    val dramUploadOrder: DramUploadOrder.Value = arrayConfig.dataflow match {
      case Dataflow.Is => DramUploadOrder.mkn
      case Dataflow.Os => DramUploadOrder.mnk
      case Dataflow.Ws => DramUploadOrder.knm
    }

    def updateStreamingDimensionSize(nextSize: Int): Architecture = copy(
      streamingDimensionSize = nextSize
    )

    def updateSramSize(nextSize: Int): Architecture = copy(
      singleBufferLimitKbs = SingleBufferLimitKbs(nextSize, nextSize, nextSize)
    )

    def updatesingleBufferLimitKb(nextSize: Int, sramType: DataType.Value): Architecture = {
      sramType match {
        case DataType.A =>
          copy(
            singleBufferLimitKbs = SingleBufferLimitKbs(
            nextSize,
            this.singleBufferLimitKbs.limitB,
            this.singleBufferLimitKbs.limitC
            )
          )

        case DataType.B =>
          copy(
            singleBufferLimitKbs = SingleBufferLimitKbs(
              this.singleBufferLimitKbs.limitA,
              nextSize,
              this.singleBufferLimitKbs.limitC
            )
          )

        case DataType.C =>
          copy(
            singleBufferLimitKbs = SingleBufferLimitKbs(
              this.singleBufferLimitKbs.limitA,
              this.singleBufferLimitKbs.limitB,
              nextSize
            )
          )
      }
    }

  }

  private case class Result(
    architecture: Architecture,
    simulationResult: SimulationResult
  )

  private val help = """
   |First argument is target MNK layer
   |Second argument is test setting argument
   |Third argument is DRAM Reference Data (Option)
   |Fourth argument is SRAM Reference Data (Option)
   |Fifth argument is Array Reference Data (Option)
  """.stripMargin

  val marginPercent = 50
  val maximumsingleBufferLimitKb = 512
  val minimumsingleBufferLimitKb = 32

  Try {
    if(args.length == 2){
      println("Running cycle-only simulation (no energy/area analysis)")
      runDesignExplorer(
        layerPath = args(0),
        testPath = args(1),
        help = help
      )
    } else if (args.length == 5){
      println("Running full simulation including cycle, energy, and area analysis")
      runDesignExplorer(
        layerPath = args(0),
        testPath = args(1),
        dramDataPath = Option(args(2)),
        sramDataPath = Option(args(3)),
        arrayDataPath = Option(args(4)),
        help = help
      )
    } else {
      Console.err.println(s"Invalid number of arguments It is ${args.length}" + help)
      sys.exit(1)
    }

  } match {
    case Success(_) =>
      log("Design Explorer Main End")
    case Failure(e) =>
      Console.err.println(s"Error: ${e.getMessage}")
      sys.exit(1)
  }

  private def runDesignExplorer(
    layerPath: String,
    testPath: String,
    dramDataPath: Option[String] = None,
    sramDataPath: Option[String] = None,
    arrayDataPath: Option[String] = None,
    help: String
  ): Unit = {

    val layerConfigParser = new ConfigManager(layerPath)
    val testConfigParser = new ConfigManager(testPath)
    val sramDataParser = sramDataPath.map(new ConfigManager(_))
    val dramDataParser = dramDataPath.map(new ConfigManager(_))
    val arrayDataParser = arrayDataPath.map(new ConfigManager(_))

    if(!layerConfigParser.parse()) {
      throw ParseError("Layer parsing failed" + help)
    }

    if(!testConfigParser.parse()){
      throw ParseError("Test option parsing failed" + help)
    }

    sramDataParser.foreach(parser =>
      if(!parser.parse()) {
        throw ParseError("SRAM Energy configuration parsing failed" + help)
      }
    )

    dramDataParser.foreach(parser =>
      if(!parser.parse()) {
        throw ParseError("DRAM Energy configuration parsing failed" + help)
      }
    )

    arrayDataParser.foreach(parser =>
      if(!parser.parse()) {
        throw ParseError("Array Energy configuration parsing failed" + help)
      }
    )

    val layerConfig = layerConfigParser.getConfig.getOrElse(
      throw ParseError("Layer config not found")
    )
    val testConfig = testConfigParser.getConfig.getOrElse(
      throw ParseError("Test Config not found")
    )

    val dramReferenceData = dramDataParser.flatMap(_.getConfig).map { config =>
      DramReferenceData(
        readEnergyPj = config.getDouble("Read Energy").getOrElse(
          throw ParseError("DRAM Read Energy Not found")
        ),
        writeEnergyPj = config.getDouble("Write Energy").getOrElse(
          throw ParseError("DRAM Write Energy Not found")
        )
      )
    }

    val sramReferenceData = sramDataParser.flatMap(_.getConfig).map(_.sramReferenceDataVector)

    val arrayReferenceData = arrayDataParser.flatMap(_.getConfig).map { config =>
      ArrayReferenceData(
        leakagePowerGroupPeRowMw = config.getDouble("Leakage Power Group PE Row").getOrElse(
          throw ParseError("Array Data Not found")
        ),
        leakagePowerGroupPeColMw = config.getDouble("Leakage Power Group PE Column").getOrElse(
          throw ParseError("Array Data Not found")
        ),
        leakagePowerVectorPeRowMw = config.getDouble("Leakage Power Vector PE Row").getOrElse(
          throw ParseError("Array Data Not found")
        ),
        leakagePowerVectorPeColMw = config.getDouble("Leakage Power Vector PE Column").getOrElse(
          throw ParseError("Array Data Not found")
        ),
        leakagePowerNumMultiplierMw = config.getDouble("Leakage Power Total Number of Multipliers").getOrElse(
          throw ParseError("Array Data Not found")
        ),
        dynamicPowerGroupPeRowPj = config.getDouble("Dynamic Power Group PE Row").getOrElse(
          throw ParseError("Array Data Not found")
        ),
        dynamicPowerGroupPeColPj = config.getDouble("Dynamic Power Group PE Column").getOrElse(
          throw ParseError("Array Data Not found")
        ),
        dynamicPowerVectorPeRowPj = config.getDouble("Dynamic Power Vector PE Row").getOrElse(
          throw ParseError("Array Data Not found")
        ),
        dynamicPowerVectorPeColPj = config.getDouble("Dynamic Power Vector PE Column").getOrElse(
          throw ParseError("Array Data Not found")
        ),
        dynamicPowerNumMultiplierPj = config.getDouble("Dynamic Power Total Number of Multipliers").getOrElse(
          throw ParseError("Array Data Not found")
        ),
        areaPowerGroupPeRowUm2 = config.getDouble("Area Group PE Row").getOrElse(
          throw ParseError("Array Data Not found")
        ),
        areaPowerGroupPeColUm2 = config.getDouble("Area Group PE Column").getOrElse(
          throw ParseError("Array Data Not found")
        ),
        areaPowerVectorPeRowUm2 = config.getDouble("Area Vector PE Row").getOrElse(
          throw ParseError("Array Data Not found")
        ),
        areaPowerVectorPeColUm2 = config.getDouble("Area Vector PE Column").getOrElse(
          throw ParseError("Array Data Not found")
        ),
        areaPowerNumMultiplierUm2 = config.getDouble("Area Total Number of Multipliers").getOrElse(
          throw ParseError("Array Data Not found")
        )
      )
    }

    val simulationConfig = buildSimulationConfig(
      layerConfig, testConfig, dramReferenceData, sramReferenceData, arrayReferenceData
    )

    val architectureArrayBuffer = buildInitialArchitecture(simConfig = simulationConfig)

    if(!simulationConfig.validate)
      throw ParseError("Invalid simulation configuration")

    val loggerOption = setLoggerOption(simulationConfig)
    logSimulation(simulationConfig)

    log("[Simulation START]")

    val optimizedArrayConfigResults = findOptimalArrayConfig(
      simConfig = simulationConfig,
      architectureBuffer = architectureArrayBuffer,
      marginPercent = marginPercent,
      loggerOption = loggerOption
    )

    if(optimizedArrayConfigResults.isEmpty){
      throw RunTimeError("There is no available optimal array configuration candidates...")
    }

    log("\t[Optimal Config Results]")
    log(s"\tTotal ${optimizedArrayConfigResults.length} have survived")

    optimizedArrayConfigResults.foreach( config => config.simulationResult.showSummary(loggerOption, config.architecture.arrayConfig.arrayConfigString))

    val optimizedStreamingDimensionResults = findOptimalStreamingSize(
      simConfig = simulationConfig,
      resultBuffer = optimizedArrayConfigResults,
      marginPercent = marginPercent,
      loggerOption = loggerOption
    )

    if(optimizedStreamingDimensionResults.isEmpty){
      throw RunTimeError("There is no available optimal streaming dimension candidates...")
    }
    log(s"")

    log("\t[Optimal Streaming Dimension Results]")
    log(s"\tTotal ${optimizedStreamingDimensionResults.length} have survived")

    optimizedStreamingDimensionResults.foreach( config => config.simulationResult.showSummary(loggerOption, config.architecture.arrayConfig.arrayConfigString))

    val optimizedSramArchitectures = findOptimalSramSize(
      simConfig = simulationConfig,
      resultBuffer = optimizedStreamingDimensionResults,
      marginPercent = marginPercent,
      loggerOption = loggerOption
    )

    if(optimizedSramArchitectures.isEmpty){
      throw RunTimeError("There is no available SRAM candidates...")
    }

    log("\t[Optimal SRAM Size Results]")
    log(s"\tTotal ${optimizedSramArchitectures.length} have survived")

    optimizedSramArchitectures.foreach( config => config.simulationResult.showSummary(loggerOption, config.architecture.arrayConfig.arrayConfigString))

    val optimizedSingleSramResults = findOptimalSingleSramSize(
      simConfig = simulationConfig,
      resultBuffer = optimizedSramArchitectures,
      marginPercent = marginPercent,
      loggerOption = loggerOption
    )

    if(optimizedSingleSramResults.isEmpty){
      throw RunTimeError("There is no available single SRAM candidates...")
    }
    log(s"")
    log("\t[Optimal Single SRAM Size Results]")
    log(s"\tTotal ${optimizedSingleSramResults.length} have survived")

    optimizedSingleSramResults.foreach( config => config.simulationResult.showSummary(loggerOption, config.architecture.arrayConfig.arrayConfigString))

    reportTopConfigurations(optimizedSingleSramResults, simulationConfig)

    log(s"")
  }

  private def setLoggerOption(simConfig: SimulationConfig): LoggerOption = {

    val outputMode = OutputMode.File
    val outputDirectory = FilePaths.resourcesOutputSimulation
    val logFileName = generateLogFileName(simConfig) + ".txt"

    val logFile = new File(outputDirectory + logFileName)
    val loggerOption = LoggerOption(outputMode, Option(logFile))
    setMode(loggerOption)

    if(logFile.exists()){
      if(logFile.delete())
        println(s"Deleted existing file: $logFileName")
      else
        println(s"Failed to delete file: $logFileName")
    }

    loggerOption

  }

  private def buildSimulationConfig(
    layerConfig: ConfigParser.Config,
    testConfig: ConfigParser.Config,
    dramReferenceData: Option[DramReferenceData] = None,
    sramReferenceData: Option[Vector[SramReferenceData]] = None,
    arrayReferenceData: Option[ArrayReferenceData] = None
  ): SimulationConfig ={

    //layer
    val layerName = layerConfig.getString("Layer Name").getOrElse(
      throw ParseError("Layer Name not found")
    )
    val dimensionM = layerConfig.getInt("M").getOrElse(
      throw ParseError("M dimension not found")
    )
    val dimensionN = layerConfig.getInt("N").getOrElse(
      throw ParseError("N dimension not found")
    )
    val dimensionK = layerConfig.getInt("K").getOrElse(
      throw ParseError("K dimension not found")
    )
    val gemmDimension = GemmDimension(dimensionM, dimensionN, dimensionK)


    //test option
    val metric = testConfig.getString("Metric").get match {
      case "Cycle" => OptimizationMetric.Cycle
      case "Area" => OptimizationMetric.Area
      case "Energy" => OptimizationMetric.Energy
      case _ =>
        throw ParseError("Invalid Optimization Metric")
    }

    val bitWidthPortA = testConfig.getInt("Port A Bit Width").getOrElse(
      throw ParseError("Port A Bit Width")
    )
    val bitWidthPortB = testConfig.getInt("Port B Bit Width").getOrElse(
      throw ParseError("Port B Bit Width")
    )

    val bitWidthPortC = testConfig.getInt("Port B Bit Width").getOrElse(
      throw ParseError("Port B Bit Width")
    )

    val dramBandwidth = testConfig.getInt("DRAM Bandwidth").getOrElse(
      throw ParseError("DRAM Bandwidth not found")
    )

    val totalNumberOfMultipliers = testConfig.getInt("Total Number of Multipliers").getOrElse(
      throw ParseError("Total number of multiplier not found")
    )

    SimulationConfig(
      layerName = layerName,
      layerGemmDimension = gemmDimension,
      metric = metric,
      bitWidthPortA = bitWidthPortA,
      bitWidthPortB = bitWidthPortB,
      bitWidthPortC = bitWidthPortC,
      dramBandwidth = dramBandwidth,
      totalNumberOfMultipliers = totalNumberOfMultipliers,
      dramReferenceData = dramReferenceData,
      sramReferenceDataVector = sramReferenceData,
      arrayReferenceData = arrayReferenceData
    )

  }


  private def buildInitialArchitecture(simConfig: SimulationConfig): ArrayBuffer[Architecture] = {
    ArrayBuffer(Dataflow.Os).flatMap{ dataflow =>
     ArrayConfigGenerator.generateArrayConfig(
       multNumber = simConfig.totalNumberOfMultipliers,
       bandWidthPortA = simConfig.bitWidthPortA,
       bandWidthPortB = simConfig.bitWidthPortB,
       dataflow = dataflow,
       streamingDimensionSize = getMaximumStreamingDimension(simConfig.layerGemmDimension, dataflow)
     )
    }.map { arrayConfig =>
      Architecture(
        arrayConfig,
        getMaximumStreamingDimension(simConfig.layerGemmDimension, arrayConfig.dataflow),
        SingleBufferLimitKbs(maximumsingleBufferLimitKb, maximumsingleBufferLimitKb, maximumsingleBufferLimitKb)
      )
    }
  }

  private def filterArchitectureByMetric(
    results: ArrayBuffer[Result],
    metric: OptimizationMetric.Value,
    marginPercent: Double = 20.0
  ): ArrayBuffer[Result] = {

    val threshold = metric match {
      case OptimizationMetric.Cycle =>
        val minCycle = results.map(_.simulationResult.cycle).min
        (minCycle * (1 + marginPercent/100)).toLong

      case OptimizationMetric.Energy =>
        val minEnergy = results.flatMap(_.simulationResult.energyPj).min
        minEnergy * (1 + marginPercent/100)

      case OptimizationMetric.Area =>
        val minArea = results.flatMap(_.simulationResult.areaMm).min
        minArea * (1 + marginPercent/100)
    }

    results.filter { result =>
      metric match {
        case OptimizationMetric.Cycle =>
          result.simulationResult.cycle <= threshold

        case OptimizationMetric.Energy =>
          result.simulationResult.energyPj.exists(_ <= threshold)

        case OptimizationMetric.Area =>
          result.simulationResult.areaMm.exists(_ <= threshold)
      }
    }
  }

  private def findOptimalArrayConfig(
    simConfig: SimulationConfig,
    architectureBuffer: ArrayBuffer[Architecture],
    marginPercent: Double = 20.0,
    loggerOption: LoggerOption
  ): ArrayBuffer[Result] = {
    val result = architectureBuffer.map{ architecture =>

      buildHardwareComponents(simConfig = simConfig, architecture = architecture, loggerOption = loggerOption) match {
        case Right(components) =>
          Result(architecture, runSimulation(simConfig, components, loggerOption))

        case Left(_) =>
          Result(architecture, SimulationResult(wrongCycle = Long.MaxValue))
      }

    }

    filterArchitectureByMetric(result, simConfig.metric ,marginPercent)
  }


  private def findOptimalStreamingSize(
    simConfig: SimulationConfig,
    resultBuffer: ArrayBuffer[Result],
    marginPercent: Double = 20.0,
    loggerOption: LoggerOption,
  ): ArrayBuffer[Result] = {

    @tailrec
    def searchStreamingDimensionSizeForArchitecture(result: Result): Result= {

      val currentArchitecture = result.architecture
      val currentStreamingDimensionSize = result.architecture.streamingDimensionSize
      val currentSimulationResult = result.simulationResult

      val nextStreamingDimensionSize = currentStreamingDimensionSize / 2
      val nextArchitecture = currentArchitecture.updateStreamingDimensionSize(nextStreamingDimensionSize)

      val nextSimulationResult = buildHardwareComponents(simConfig, nextArchitecture, loggerOption) match {
        case Right(components) =>
          runSimulation(simConfig, components, loggerOption)

        case Left(_) =>
          SimulationResult(wrongCycle = Long.MaxValue)

      }

      val newResult = Result(nextArchitecture, nextSimulationResult)

      if (nextSimulationResult.cycle <= currentSimulationResult.cycle) {
        searchStreamingDimensionSizeForArchitecture(newResult)
      } else {
        result
      }

    }

    val result = resultBuffer
      .map(result =>searchStreamingDimensionSizeForArchitecture(result))

    filterArchitectureByMetric(result, simConfig.metric, marginPercent)

  }

  private def findOptimalSramSize(
    simConfig: SimulationConfig,
    resultBuffer: ArrayBuffer[Result],
    marginPercent: Double = 20.0,
    loggerOption: LoggerOption,
  ): ArrayBuffer[Result] = {

    @tailrec
    def searchSramForArchitecture(result: Result): Result = {

      val currentArchitecture = result.architecture
      val currentSramSize = result.architecture.singleBufferLimitKbs.limitA
      val currentSimulationResult = result.simulationResult

      assert(currentArchitecture.singleBufferLimitKbs.isAllSramSizeSame,
        "Search SRAM for archi functions is called in wrong time")

      val nextSramSize = currentSramSize / 2

      if(nextSramSize < minimumsingleBufferLimitKb){
        result
      } else {

        val nextArchitecture = currentArchitecture.updateSramSize(nextSramSize)
        val nextSimulationResult = buildHardwareComponents(simConfig, nextArchitecture, loggerOption) match {
          case Right(components) =>
            runSimulation(simConfig,components, loggerOption)
          case Left(_) =>
            SimulationResult(wrongCycle = Long.MaxValue)
        }

        val newResult = Result(nextArchitecture, nextSimulationResult)

        if (nextSimulationResult.cycle <= currentSimulationResult.cycle)
          searchSramForArchitecture(newResult)
        else
          result

      }

    }

    val result = resultBuffer.map(architectureResult => searchSramForArchitecture(architectureResult))
    filterArchitectureByMetric(result, simConfig.metric, marginPercent)

  }


  private def findOptimalSingleSramSize(
    simConfig: SimulationConfig,
    resultBuffer: ArrayBuffer[Result],
    marginPercent: Double = 20.0,
    loggerOption: LoggerOption,
  ): ArrayBuffer[Result] = {

    @tailrec
    def searchSingleSramForArchitecture(result: Result): Result = {

      val currentArchitecture = result.architecture

      val currentSramSize = currentArchitecture.arrayConfig.dataflow match {
        case Dataflow.Is =>
          result.architecture.singleBufferLimitKbs.limitA
        case Dataflow.Os =>
          result.architecture.singleBufferLimitKbs.limitC
        case Dataflow.Ws =>
          result.architecture.singleBufferLimitKbs.limitB
      }

      val currentResult = result.simulationResult
      val nextSramSize = currentSramSize / 2

      if(nextSramSize < minimumsingleBufferLimitKb){
        result
      } else {

        val nextArchitecture = currentArchitecture.arrayConfig.dataflow match {
          case Dataflow.Is =>
            currentArchitecture.updatesingleBufferLimitKb(nextSramSize, DataType.A)
          case Dataflow.Os =>
            currentArchitecture.updatesingleBufferLimitKb(nextSramSize, DataType.C)
          case Dataflow.Ws =>
            currentArchitecture.updatesingleBufferLimitKb(nextSramSize, DataType.B)
        }

        val newResult = buildHardwareComponents(simConfig, nextArchitecture, loggerOption) match {
          case Right(components) =>
            runSimulation(simConfig, components, loggerOption)

          case Left(_) =>
            SimulationResult(wrongCycle = Long.MaxValue)
        }

        val newArchitectureResult = Result(nextArchitecture, newResult)

        if (newResult.cycle <= currentResult.cycle)
          searchSingleSramForArchitecture(newArchitectureResult)
        else
          result

      }

    }

    val result = resultBuffer.map(result => searchSingleSramForArchitecture(result))
    filterArchitectureByMetric(result, simConfig.metric, marginPercent)

  }

  private def reportTopConfigurations(
    optimizedResults: ArrayBuffer[Result],
    simConfig: SimulationConfig,
  ): Unit = {
    log("\n[Top 3 Hardware Configurations Report]")

    // Sort results based on optimization metric
    val sortedResults = simConfig.metric match {
      case OptimizationMetric.Cycle =>
        optimizedResults.sortBy(_.simulationResult.cycle)
      case OptimizationMetric.Energy =>
        optimizedResults.sortBy(r => r.simulationResult.energyPj.getOrElse(Double.MaxValue))
      case OptimizationMetric.Area =>
        optimizedResults.sortBy(r => r.simulationResult.areaMm.getOrElse(Double.MaxValue))
    }

    // Take top 3 configurations
    val topThree = sortedResults.take(3)

    log(s"Optimization Metric: ${simConfig.metric}")

    log(s"")
    topThree.zipWithIndex.foreach { case (result, index) =>
      log(s"\t[Rank ${index + 1}]")

      // Configuration details
      val config = result.architecture.arrayConfig
      log(s"\tArray Configuration: ${config.arrayConfigString}")
      log(s"\tGroup PE (Row x Col): ${config.groupPeRow} x ${config.groupPeCol}")
      log(s"\tVector PE (Row x Col): ${config.vectorPeRow} x ${config.vectorPeCol}")
      log(s"\tNumber of Multipliers: ${config.numMultiplier}")
      log(s"\tStreaming Dimension Size: ${result.architecture.streamingDimensionSize}")

      // Buffer sizes
      val bufferLimits = result.architecture.singleBufferLimitKbs
      log(s"\tBuffer Sizes (KB):")
      log(s"\t\tSRAM A: ${bufferLimits.limitA}")
      log(s"\t\tSRAM B: ${bufferLimits.limitB}")
      log(s"\t\tSRAM C: ${bufferLimits.limitC}")

      // Performance metrics
      val simResult = result.simulationResult
      log("\n\tPerformance Metrics:")
      log(s"\t\tCycle Count: ${simResult.cycle}")

      simResult.energyPj.foreach { energy =>
        log(s"\t\tTotal Energy: ${String.format("%.2f", energy)} pJ")
      }

      simResult.areaMm.foreach { area =>
        log(s"\t\tTotal Area: ${String.format("%.2f", area)} mm²")
      }
      log(s"")
    }
  }

  private def buildHardwareComponents(
    simConfig: DesignExplorerMain.SimulationConfig,
    architecture: Architecture,
    loggerOption: LoggerOption,
  ):Either[CompBuildError, (Layer, Dram, (DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram), Array, Interface)] = {


    val layer = new Layer(
      layerName = simConfig.layerName,
      gemmDimension = simConfig.layerGemmDimension,
      arrayConfig = architecture.arrayConfig,
      streamingDimensionSize = architecture.streamingDimensionSize,
      dramUploadOrder = architecture.dramUploadOrder,
      loggerOption = loggerOption
    )

    val sizeTileA = layer.operationVector.head.generateTileA.dims.memorySize
    val sizeTileB = layer.operationVector.head.generateTileB.dims.memorySize
    val sizeTileC = layer.operationVector.head.generateTileC.dims.memorySize

    val dram = new Dram(
      outputBandwidth = simConfig.dramBandwidth, loggerOption = loggerOption
    )

    val array = new Array(
      arrayConfig = architecture.arrayConfig,
      loggerOption = loggerOption
    )

    buildSrams(
      arrayConfig = architecture.arrayConfig,
      simConfig = simConfig,
      singleBufferLimitKbs = architecture.singleBufferLimitKbs,
      sizeTileA = sizeTileA,
      sizeTileB = sizeTileB,
      sizeTileC = sizeTileC,
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
        Right((layer, dram, srams, array, interface))


      case Left(error) =>
        println(s"Failed to build hardware components array configuration: ${architecture.arrayConfig.arrayConfigString}")
        Left(error)

    }


  }

  private def buildSrams(
    arrayConfig: ArrayConfig,
    simConfig: SimulationConfig,
    singleBufferLimitKbs: SingleBufferLimitKbs,
    sizeTileA: Int,
    sizeTileB: Int,
    sizeTileC: Int,
    loggerOption: LoggerOption
  ): Either[CompBuildError,(DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram)] = {

    val capacityA = singleBufferLimitKbs.limitBitA / sizeTileA
    val capacityB = singleBufferLimitKbs.limitBitB / sizeTileB
    val capacityC = singleBufferLimitKbs.limitBitC / sizeTileC

    if(!(capacityA > 0 && capacityB > 0 && capacityC > 0 )) {
      println(s"Building SRAM is failed: ${arrayConfig.arrayConfigString}")
      Left(CompBuildError("SRAM Cannot contain even 1 tile increase SRAM size"))
    } else {
      val sramA = new DoubleBufferSram(
        dataType = DataType.A,
        outputBandwidth = arrayConfig.bandwidthOfInputA,
        singleBufferTileCapacity = capacityA,
        singleBufferLimitKb = singleBufferLimitKbs.limitA,
        loggerOption = loggerOption
      )
      val sramB = new DoubleBufferSram(
        dataType = DataType.B,
        outputBandwidth = arrayConfig.bandwidthOfInputB,
        singleBufferTileCapacity = capacityB,
        singleBufferLimitKb = singleBufferLimitKbs.limitB,
        loggerOption = loggerOption
      )
      val sramC = new OutputDoubleBufferSram(
        outputBandwidth = simConfig.dramBandwidth,
        singleBufferTileCapacity = capacityC,
        singleBufferLimitKb = singleBufferLimitKbs.limitC,
        loggerOption = loggerOption
      )

      Right((sramA, sramB, sramC))

    }


  }

  private def runSimulation(
    simConfig: SimulationConfig,
    components: (Layer, Dram, (DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram), Array, Interface),
    loggerOption: LoggerOption,
  ):  SimulationResult = {
    val (layer, dram, srams, array, interface) = components
    val (sramA, sramB, sramC) = srams

    val simulation = new SystemSimulator(
      dram = dram,
      sramA = sramA,
      sramB = sramB,
      sramC = sramC,
      interface = interface,
      layer = layer,
      array = array,
      dramReferenceData = simConfig.dramReferenceData,
      sramDataReferenceVector = simConfig.sramReferenceDataVector,
      arrayReferenceData = simConfig.arrayReferenceData,
      loggerOption = loggerOption,
    )


    try {
      simulation.startSimulation()

      SimulationResult(
        totalOperationNumber = simulation.getTotalOperationNumber,
        tileSizeA = simulation.getTileSizeA,
        tileSizeB = simulation.getTileSizeB,
        tileSizeC = simulation.getTileSizeC,
        trimTileCountA = simulation.getTrimTileCountA,
        trimTileCountB = simulation.getTrimTileCountB,
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

        sramAreaMmA = simulation.getSramAreaA,
        sramAreaMmB = simulation.getSramAreaB,
        sramAreaMmC = simulation.getSramAreaC,

        arrayAreaMm = simulation.getArrayArea,
        areaMm = simulation.getArea

      )

    } catch {
      case _: RunTimeError =>
        SimulationResult(wrongCycle = Long.MaxValue)
    }

  }

  private def generateLogFileName(config: SimulationConfig): String = {
    s"/result_${config.layerName}_dram_bandwidth:${config.dramBandwidth}_mult:${config.totalNumberOfMultipliers}"
  }

  private def logSimulation(simConfig: SimulationConfig): Unit = {

    log("[Design Explorer Simulation Overview]")
    log("\t[Target Layer]")
    log(s"\t\tLayer Name: ${simConfig.layerName}")
    log(s"\t\tM: ${simConfig.layerGemmDimension.m}, " +
      s"N: ${simConfig.layerGemmDimension.n}, " +
      s"K: ${simConfig.layerGemmDimension.k} ")
    log("")
    log(s"\t[Optimization Metric]")
    log(s"\t\tMetric: ${simConfig.metric}")
    log(s"")
    log(s"\t[DRAM]")
    log(s"\t\tDRAM Bandwidth: ${simConfig.dramBandwidth}")
    log("")
    log(s"[Systolic Tensor Array]")
    log(s"\t\tPort A Bit Width: ${simConfig.bitWidthPortA}")
    log(s"\t\tPort B Bit Width: ${simConfig.bitWidthPortB}")
    log(s"\t\tTotal Number of Multipliers: ${simConfig.totalNumberOfMultipliers}")
    log("")
  }

}


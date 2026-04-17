package simulation

import common.{Dataflow, OutputPortCalculator}

import java.io.File
import scala.collection.mutable.ArrayBuffer
import common.FilePaths

import scala.util.{Failure, Success, Try}

object SystemArchitectureOptimizer extends App with Logger with StreamingDimensionCalculator with OutputPortCalculator {

  object OptimizationMetric extends Enumeration{
    type OptimizationMetric = Value
    val Cycle, Energy, Area, TOPS = Value
  }

  case class SimulationConfig(
    layerName: String,
    layerGemmDimension: GemmDimension,
    metric: OptimizationMetric.Value,
    bitWidthPortA: Int,
    bitWidthPortB: Int,
    bitWidthPortC: Int,
    offChipMemoryBandwidth: Int,
    totalNumberOfMultipliers: Int,
    offChipMemoryReferenceData: Option[OffChipMemoryReferenceData],
    sramReferenceDataVector: Option[Vector[SramReferenceData]],
    retentionRatio1: Option[Double],
    retentionRatio2: Option[Double],
    retentionRatio3: Option[Double],

  ) {

    def validate: Boolean = {
      val baseValidation = layerGemmDimension.validate &&
        totalNumberOfMultipliers > 0 &&
        bitWidthPortA > 0 &&
        bitWidthPortB > 0 &&
        bitWidthPortC > 0 &&
        offChipMemoryBandwidth > 0

      val energyValidation = (offChipMemoryReferenceData, sramReferenceDataVector) match {
        case (Some(offChipMemory), Some(sram)) =>
          offChipMemory.validate && sram.forall(_.validate)
        case (None, None) =>
          true
        case _ =>
          false
      }

      baseValidation && energyValidation

    }
  }


  private val help = """
    |Usage:
    |[2 arguments] - Cycle Report Only Mode:
    |  First argument is target MNK layer
    |  Second argument is test setting argument
    |
    |[4 arguments] - Cycle and Energy Report Mode with Few Shot Prediction for Array Synthesis Data:
    |  First argument is target MNK layer
    |  Second argument is test setting argument
    |  Third argument is off chip memory Reference Data
    |  Fourth argument is SRAM Reference Data
    |
  """.stripMargin

  private val maximumSingleBufferLimitKb: Int = 512
  private val minimumSingleBufferLimitKb: Int = 32

  println("=" * 30 + "System Architecture Optimizer START" + "=" * 30)

  Try {
    if(args.length == 2){
      println("Running cycle-only simulation (no energy/area analysis)")
      run(
        layerPath = args(0),
        testPath = args(1),
        help = help
      )
    } else if (args.length == 4){
      println("Running full simulation including cycle, energy, and area analysis")
      run(
        layerPath = args(0),
        testPath = args(1),
        offChipMemoryDataPath = Option(args(2)),
        sramDataPath = Option(args(3)),
        help = help
      )
    } else {
      Console.err.println(s"Invalid number of arguments It is ${args.length}" + help)
      sys.exit(1)
    }

  } match {
    case Success(_) =>
      println("=" * 30 + "System Architecture Optimizer END" + "=" * 30)
    case Failure(e) =>
      Console.err.println(s"Error: ${e.getMessage}")
      sys.exit(1)
  }

  private def run(
    layerPath: String,
    testPath: String,
    offChipMemoryDataPath: Option[String] = None,
    sramDataPath: Option[String] = None,
    help: String
  ): Unit = {

    //Parsing
    val layerConfigParser = new ConfigManager(layerPath)
    val testConfigParser = new ConfigManager(testPath)
    val sramDataParser = sramDataPath.map(new ConfigManager(_))
    val offChipMemoryDataParser = offChipMemoryDataPath.map(new ConfigManager(_))

    println("Parsing START")

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

    offChipMemoryDataParser.foreach(parser =>
      if(!parser.parse()) {
        throw ParseError("Off Chip Memory Energy configuration parsing failed" + help)
      }
    )

    val layerConfig = layerConfigParser.getConfig.getOrElse(
      throw ParseError("Layer config not found")
    )
    val testConfig = testConfigParser.getConfig.getOrElse(
      throw ParseError("Test Config not found")
    )

    val offChipMemoryReferenceData = offChipMemoryDataParser.flatMap(_.getConfig).map { config =>
      OffChipMemoryReferenceData(
        readEnergyPj = config.getDouble("Read Energy").getOrElse(
          throw ParseError("Off Chip Memory Read Energy Not found")
        ),
        writeEnergyPj = config.getDouble("Write Energy").getOrElse(
          throw ParseError("Off Chip Memory Write Energy Not found")
        )
      )
    }

    val sramReferenceData = sramDataParser.flatMap(_.getConfig).map(_.sramReferenceDataVector)


    if (!FewShotPredictor.isModelLoaded) {
//      println("📥 Loading ML prediction model...")
      FewShotPredictor.loadModelFromDefaultFiles match {
        case Success(_) =>
          println("✅ Model loaded successfully")
        case Failure(e) =>
          println(s"❌ Failed to load model: ${e.getMessage}")
          throw new RuntimeException(s"Could not load prediction model: ${e.getMessage}")
      }
    }


    println("Parsing END")

    //build simulation config
    println("Building Simulation Config START")
    val simulationConfig = buildSimulationConfig(
     layerConfig,
      testConfig,
      offChipMemoryReferenceData,
      sramReferenceData,
    )
    println("Building Simulation Config END")

    //build initial architecture
    println("Building Initial Architecture START")

    val architectureCandidates = buildInitialArchitecture(simulationConfig)

    if(!simulationConfig.validate)
      throw ParseError("Invalid Simulation Configurations")

    val loggerOption = setLoggerOption(simulationConfig)
    logSimulation(simulationConfig)

    println("Building Initial Architecture END")

    println("[Architecture Evaluator START]")

    val architectureOptimizer = new ArchitectureOptimizer(
      simConfig = simulationConfig,
      architectureCandidates = architectureCandidates,
      minSramSize = minimumSingleBufferLimitKb,
      loggerOption = loggerOption
    )

    architectureOptimizer.run()
    architectureOptimizer.logTopResults()
    architectureOptimizer.logTopResultsCsv()
    architectureOptimizer.logTopEasyResultsCsv()
    println("[Architecture Evaluator END]")

  }

  private def buildSimulationConfig(
    layerConfig: ConfigParser.Config,
    testConfig: ConfigParser.Config,
    offChipMemoryReferenceData: Option[OffChipMemoryReferenceData] = None,
    sramReferenceData: Option[Vector[SramReferenceData]] = None,
  ): SimulationConfig = {

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
      case "TOPS" => OptimizationMetric.TOPS
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

    val offChipMemoryBandwidth = testConfig.getInt("Off Chip Memory Bandwidth").getOrElse(
      throw ParseError("Off Chip Memory Bandwidth not found")
    )

    val totalNumberOfMultipliers = testConfig.getInt("Total Number of Multipliers").getOrElse(
      throw ParseError("Total number of multiplier not found")
    )

    val retentionRatio1 = testConfig.getDouble("Retention Ratio1")
    val retentionRatio2 = testConfig.getDouble("Retention Ratio2")
    val retentionRatio3 = testConfig.getDouble("Retention Ratio3")

    SimulationConfig(
      layerName = layerName,
      layerGemmDimension = gemmDimension,
      metric = metric,
      bitWidthPortA = bitWidthPortA,
      bitWidthPortB = bitWidthPortB,
      bitWidthPortC = bitWidthPortC,
      offChipMemoryBandwidth = offChipMemoryBandwidth,
      totalNumberOfMultipliers = totalNumberOfMultipliers,
      offChipMemoryReferenceData = offChipMemoryReferenceData,
      sramReferenceDataVector = sramReferenceData,
      retentionRatio1 = retentionRatio1,
      retentionRatio2 = retentionRatio2,
      retentionRatio3 = retentionRatio3,
    )

  }

  private def buildInitialArchitecture(simConfig: SimulationConfig): ArrayBuffer[Architecture] = {
    ArrayBuffer(Dataflow.Is, Dataflow.Os, Dataflow.Ws).flatMap{ dataflow =>

      ArrayConfigGenerator.generateArrayConfig(
        multNumber = simConfig.totalNumberOfMultipliers,
        bitWidthPortA = simConfig.bitWidthPortA,
        bitWidthPortB = simConfig.bitWidthPortB,
        dataflow = dataflow,
        streamingDimensionSize = getMaximumStreamingDimension(simConfig.layerGemmDimension, dataflow),
        isRtlOnly = false
      )

    }.map { arrayConfig =>
      Architecture(
        arrayConfig,
        getMaximumStreamingDimension(simConfig.layerGemmDimension, arrayConfig.dataflow),
        maximumSingleBufferLimitKb,
        maximumSingleBufferLimitKb,
        maximumSingleBufferLimitKb
      )
    }

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

  private def generateLogFileName(config: SimulationConfig): String = {
    if(config.retentionRatio1.isDefined && config.retentionRatio2.isDefined && config.retentionRatio3.isDefined)
      s"/result_${config.layerName}_mult:${config.totalNumberOfMultipliers}" +
        s"_${config.retentionRatio1}" +
        s"_${config.retentionRatio2}" +
        s"_${config.retentionRatio3}"
    else
      s"/result_${config.layerName}_mult:${config.totalNumberOfMultipliers}"
  }

  private def logSimulation(simConfig: SimulationConfig): Unit = {

    log("[Design Explorer Simulation Overview]")
    log("\t[Optimization Metric]")
    log(s"\t\t${simConfig.metric}\n")
    log("\t[Target Layer]")
    log(s"\t\tLayer Name: ${simConfig.layerName}")
    log(s"\t\tM: ${simConfig.layerGemmDimension.m}, " +
      s"N: ${simConfig.layerGemmDimension.n}, " +
      s"K: ${simConfig.layerGemmDimension.k} ")
    log(s"")
    log(s"\t[Off Chip Memory]")
    log(s"\t\tOff Chip Memory Bandwidth: ${simConfig.offChipMemoryBandwidth}")
    log("")
    log(s"[Systolic Tensor Array]")
    log(s"\t\tPort A Bit Width: ${simConfig.bitWidthPortA}")
    log(s"\t\tPort B Bit Width: ${simConfig.bitWidthPortB}")
    log(s"\t\tTotal Number of Multipliers: ${simConfig.totalNumberOfMultipliers}")
    log("")
  }

}

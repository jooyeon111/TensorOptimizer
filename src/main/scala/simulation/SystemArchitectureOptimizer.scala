package simulation

import common.{Dataflow, OutputPortCalculator}

import java.io.File
import scala.collection.mutable.ArrayBuffer
import common.FilePaths

import scala.util.{Failure, Success, Try}

object SystemArchitectureOptimizer extends App with Logger with StreamingDimensionCalculator with OutputPortCalculator {

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
    dnnModelWeightsPath: Option[String],
  ) {

    def validate: Boolean = {
      val baseValidation = layerGemmDimension.validate &&
        totalNumberOfMultipliers > 0 &&
        bitWidthPortA > 0 &&
        bitWidthPortB > 0 &&
        bitWidthPortC > 0 &&
        dramBandwidth > 0

      val energyValidation = (dramReferenceData, sramReferenceDataVector) match {
        case (Some(dram), Some(sram)) =>
          dram.validate && sram.forall(_.validate)
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
    |[5 arguments] - Cycle and Energy Report Mode with ML inference for Array Synthesis Data:
    |  First argument is target MNK layer
    |  Second argument is test setting argument
    |  Third argument is DRAM Reference Data
    |  Fourth argument is SRAM Reference Data
    |  Fifth argument is ML weight file (.bin)
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
    } else if (args.length == 5){
      println("Running full simulation including cycle, energy, and area analysis")
      run(
        layerPath = args(0),
        testPath = args(1),
        dramDataPath = Option(args(2)),
        sramDataPath = Option(args(3)),
        dnnModelWeightsPath = Option(args(4)),
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
    dramDataPath: Option[String] = None,
    sramDataPath: Option[String] = None,
    dnnModelWeightsPath: Option[String]= None,
    help: String
  ): Unit = {

    //Parsing
    val layerConfigParser = new ConfigManager(layerPath)
    val testConfigParser = new ConfigManager(testPath)
    val sramDataParser = sramDataPath.map(new ConfigManager(_))
    val dramDataParser = dramDataPath.map(new ConfigManager(_))

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

    dramDataParser.foreach(parser =>
      if(!parser.parse()) {
        throw ParseError("DRAM Energy configuration parsing failed" + help)
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

    println("Parsing END")

    //build simulation config
    println("Building Simulation Config START")
    val simulationConfig = buildSimulationConfig(
     layerConfig, testConfig, dramReferenceData, sramReferenceData, dnnModelWeightsPath
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

    val architectureEvaluator = new ArchitectureEvaluator(
      simConfig = simulationConfig,
      architectureCandidates = architectureCandidates,
      minSramSize = minimumSingleBufferLimitKb,
      loggerOption = loggerOption
    )

    architectureEvaluator.run()
    architectureEvaluator.logTopResults()
    println("[Architecture Evaluator END]")

  }

  private def buildSimulationConfig(
    layerConfig: ConfigParser.Config,
    testConfig: ConfigParser.Config,
    dramReferenceData: Option[DramReferenceData] = None,
    sramReferenceData: Option[Vector[SramReferenceData]] = None,
    dnnModelWeightsPath: Option[String] = None,
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
      dnnModelWeightsPath= dnnModelWeightsPath,
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
        dnnModelWeightsPath = simConfig.dnnModelWeightsPath
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
    s"/result_${config.layerName}_dram_bandwidth:${config.dramBandwidth}_mult:${config.totalNumberOfMultipliers}"
  }

  private def logSimulation(simConfig: SimulationConfig): Unit = {

    log("[Design Explorer Simulation Overview]")
    log("\t[Metric]")
    log(s"\t\t${simConfig.metric}")
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
//private case class ArchitectureResult(
//                                           architecture: Architecture,
//                                           simulationResult: SimulationResult
//                                         ) {
//  def showSummary(): Unit = {
//    if(simulationResult.isEnergyReportValid && simulationResult.isAreaReportValid){
//      log(s"\t[${architecture.arrayConfig.arrayConfigString}]")
//      log(s"\t\tCycle: ${simulationResult.cycle}")
//      log(s"\t\tArea: ${String.format("%.2f", simulationResult.areaUm2.get)} mm^2")
//      log(s"\t\tEnergy: ${String.format("%.2f", simulationResult.energyPj.get)} pJ")
//      log(s"\t\tStreaming Dimension Size: ${architecture.streamingDimensionSize}")
//      log(s"\t\tSingleBuffer A: ${architecture.singleBufferLimitKbA} KB")
//      log(s"\t\tSingleBuffer B: ${architecture.singleBufferLimitKbB} KB")
//      log(s"\t\tSingleBuffer C: ${architecture.singleBufferLimitKbC} KB\n")
//    } else {
//      log(s"\t[$architecture.arrayConfig.arrayConfigString]")
//      log(s"\t\tCycle: ${simulationResult.cycle},")
//      log(s"\t\tStreaming Dimension Size: ${architecture.streamingDimensionSize}")
//      log(s"\t\tSingleBuffer A: ${architecture.singleBufferLimitKbA} KB")
//      log(s"\t\tSingleBuffer B: ${architecture.singleBufferLimitKbB} KB")
//      log(s"\t\tSingleBuffer C: ${architecture.singleBufferLimitKbC} KB\n")
//    }
//  }
//}
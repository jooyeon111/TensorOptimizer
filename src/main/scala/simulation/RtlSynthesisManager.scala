package simulation

import java.io.File
import common.{Dataflow, FilePaths, IntegerType, OutputPortCalculator, VerilogGenerationConfig, RtlGenerator}

trait RtlSynthesisManager extends BandWidthFilter with OutputPortCalculator with RtlGenerator with Logger {

  private case class BandwidthConfig(
    bitWidthPortA: Int,
    bitWidthPortB: Int,
    totalNumberOfMultiplier: Int,
    streamingDimensionSize: Int,
    verilogGeneration: Boolean,
    splitVerilogModules: Boolean,
  )

  def processArrayConfigsAndGenerateRtl(bandwidthInfoPath: String, help: String): Unit = {

    val config = parseBandwidthConfig(bandwidthInfoPath, help)

    val dataflows = Vector(Dataflow.Is, Dataflow.Os, Dataflow.Ws)
    dataflows.foreach { dataflow =>
      val arrayConfigs = ArrayConfigGenerator.generateArrayConfig(
        config.totalNumberOfMultiplier,
        config.bitWidthPortA,
        config.bitWidthPortB,
        dataflow,
        config.streamingDimensionSize
      )

      setupLogging(dataflow, config.totalNumberOfMultiplier)
      val filteredConfigs = filterConfigsWithTwoSigma(arrayConfigs)

      filteredConfigs.foreach { arrayConfig =>
        generateVerilogForConfig(arrayConfig, config.streamingDimensionSize, config.verilogGeneration, config.splitVerilogModules )
      }

      showBandwidthComparison(arrayConfigs, filteredConfigs)

    }
  }

  private def parseBandwidthConfig(path: String, help: String) = {

    val bandwidthConfigParser = new ConfigManager(path)

    if(!bandwidthConfigParser.parse()){
      throw ParseError("Bandwidth info parsing failed" + help)
    }

    val bandwidthInfoConfig = bandwidthConfigParser.getConfig.getOrElse(
      throw ParseError("Bandwidth info not found")
    )

    BandwidthConfig(
      bitWidthPortA = bandwidthInfoConfig.getInt("Port A Bit Width").getOrElse(
        throw ParseError("Port A Bit Width not found")
      ),
      bitWidthPortB = bandwidthInfoConfig.getInt("Port B Bit Width").getOrElse(
        throw ParseError("Port B Bit Width not found")
      ),
      totalNumberOfMultiplier = bandwidthInfoConfig.getInt("Total Number of Multipliers").getOrElse(
        throw ParseError("Total number of multiplier not found")
      ),
      streamingDimensionSize = bandwidthInfoConfig.getInt("Streaming Dimension Size").getOrElse(
        throw ParseError("Streaming Dimension Size not found")
      ),
      verilogGeneration = bandwidthInfoConfig.getBoolean("Verilog Generation").getOrElse(
        throw ParseError("Verilog Generation not found")
      ),
      splitVerilogModules = bandwidthInfoConfig.getBoolean("Split Verilog Modules").getOrElse(
        throw ParseError("Split Verilog Modules not found")
      )

    )

  }

  private def showBandwidthComparison(
    originalConfigs: Vector[ArrayConfig],
    filteredConfigs: Vector[ArrayConfig]
  ): Unit = {

    log(s"\n[Configuration Candidates]")
    showConfigBandwidths(originalConfigs, "created")

    log(s"\n[After Configurations trimming with singma-2]")
    showConfigBandwidths(filteredConfigs, "survived")

  }

  private def showConfigBandwidths(configs: Vector[ArrayConfig], resultType: String): Unit = {
    configs.foreach(config =>log(
      s"\t[${config.arrayConfigString}] " +
        s"\tInput Bandwidth A: ${config.bandwidthOfInputA} " +
        s"\tInput Bandwidth B: ${config.bandwidthOfInputB} " +
        s"\tOutput Bandwidth C: ${config.outputBandwidth}"
    ))

    log(s"")
    log(s"${configs.length} have been $resultType")
    log(s"Minimum value input bandwidth A: ${configs.map(_.bandwidthOfInputA).min}")
    log(s"Minimum value input bandwidth B: ${configs.map(_.bandwidthOfInputB).min}")
    log(s"Minimum value output bandwidth C: ${configs.map(_.outputBandwidth).min}")
    log(s"Max value input bandwidth A: ${configs.map(_.bandwidthOfInputA).max}")
    log(s"Max value input bandwidth B: ${configs.map(_.bandwidthOfInputB).max}")
    log(s"Max value output bandwidth C: ${configs.map(_.outputBandwidth).max}")
    log(s"")

  }

  private def generateVerilogForConfig(
    config: ArrayConfig,
    streamingDimensionSize: Int,
    verilogGeneration: Boolean,
    splitVerilogModules: Boolean,
  ): Unit = {
    val verilogConfig = VerilogGenerationConfig(
      splitVerilogOutput = splitVerilogModules,
      dataflow = config.dataflow,
      arrayDimension = config.asArrayDimension,
      integerType = IntegerType.Signed,
      portBitWidthInfo = calculatePortBitWidthInfo(
        dataflow = config.dataflow,
        groupPeRow = config.groupPeRow,
        groupPeCol = config.groupPeCol,
        vectorPeRow = config.vectorPeRow,
        vectorPeCol = config.vectorPeCol,
        numMultiplier = config.numMultiplier,
        bitWidthPortA = config.portBitWidth.typeA,
        bitWidthPortB = config.portBitWidth.typeB,
        configPortC = None,
        streamingDimensionSize = streamingDimensionSize
      )
    )
    if(verilogGeneration)
      generateRtl(verilogConfig)
  }


  private def setupLogging(dataflow: Dataflow.Value, totalMultipliers: Int): Unit = {
    val dataflowName = dataflow.toString.toLowerCase
    val logFileName = s"/bandwidth_analyzer_${dataflowName}_multiplier_$totalMultipliers.txt"
    val logFile = new File(FilePaths.resourcesOutputSimulation + logFileName)
    val loggerOption = LoggerOption(OutputMode.File, Option(logFile))
    setMode(loggerOption)
  }

}

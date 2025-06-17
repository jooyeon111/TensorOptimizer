package simulation

import java.io.File
import common.{Dataflow, FilePaths, IntegerType, OutputPortCalculator, VerilogGenerationConfig, RtlGenerator}

trait RtlGenerationManager extends OutputPortCalculator with RtlGenerator with Logger {

  private case class BandwidthConfig(
    bitWidthPortA: Int,
    bitWidthPortB: Int,
    totalNumberOfMultiplier: Int,
    streamingDimensionSize: Int,
    verilogGeneration: Boolean,
    splitVerilogModules: Boolean,
  )

  def generateRtl(bandwidthInfoPath: String, help: String): Unit = {

    val config = parseBandwidthConfig(bandwidthInfoPath, help)

//    val dataflows = Vector(Dataflow.Is, Dataflow.Os, Dataflow.Ws)
    val dataflows = Vector(Dataflow.Is)
    dataflows.foreach { dataflow =>
      val arrayConfigs = ArrayConfigGenerator.generateArrayConfig(
        config.totalNumberOfMultiplier,
        config.bitWidthPortA,
        config.bitWidthPortB,
        dataflow,
        config.streamingDimensionSize,
        isRtlOnly = true
      )
      arrayConfigs.foreach { arrayConfig =>
        generateVerilogForConfig(arrayConfig, config.streamingDimensionSize, config.verilogGeneration, config.splitVerilogModules )
      }
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
      ),
      streamingDimensionSize = streamingDimensionSize
    )
    if(verilogGeneration)
      generateRtl(verilogConfig)
  }

}

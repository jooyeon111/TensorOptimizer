package simulation

import java.io.File
import common.{Dataflow, OutputPortCalculator, FilePaths, ArrayDimension}

trait SingleLayerSimulation extends OutputPortCalculator with Logger with StreamingDimensionCalculator {

  private case class SimulationConfig(
    debugPrint: Boolean,
    debugStartCycle: Int,
    debugEndCycle: Int,
    layerName: String,
    layerGemmDimension: GemmDimension,
    streamingDimensionSize: Int,
    dataflow: Dataflow.Value,
    groupPeRow: Int,
    groupPeCol: Int,
    vectorPeRow: Int,
    vectorPeCol: Int,
    numMultiplier: Int,
    portBitWidth: PortBitWidth,
    dramBandwidth: Int,
    dramUploadOrder: DramUploadOrder.Value,
    singleBufferLimitKbA: Int,
    singleBufferLimitKbB: Int,
    singleBufferLimitKbC: Int,

    dramReferenceData: Option[DramReferenceData],
    sramReferenceDataVector: Option[Vector[SramReferenceData]],
    arrayReferenceData: Option[ArrayReferenceData],

  ) {
    def validate: Boolean = {
      val baseValidation = streamingDimensionSize > 0 &&
        groupPeRow > 0 &&
        groupPeCol > 0 &&
        vectorPeRow > 0 &&
        vectorPeCol > 0 &&
        numMultiplier > 0 &&
        dramBandwidth > 0 &&
        singleBufferLimitKbA > 0 &&
        singleBufferLimitKbB > 0 &&
        singleBufferLimitKbC > 0 &&
        portBitWidth.validate &&
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

  //TODO reorder parameters
  def processOneLayer(
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

    val simulationConfig = buildSimulationOneLayerConfig(
      layerConfig = layerConfig,
      testConfig = testConfig,
      dramReferenceData = dramReferenceData,
      sramReferenceData = sramReferenceData,
      arrayReferenceData = arrayReferenceData
    )

    if(!simulationConfig.validate)
      throw ParseError("Invalid simulation configuration")

    println("Initialize Components")
    val (components, loggerOption) = initializeComponents(simulationConfig)

    logSimulation(simulationConfig)

    println("Run Simulation")
    val simResult = runSimulation(simulationConfig, components)

    simResult.printFullResults(loggerOption)
//    simResult.showSummary(loggerOption)

  }

  private def buildSimulationOneLayerConfig(
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

    //test configuration
    val debugPrint = testConfig.getBoolean("Debug Print").getOrElse(false)
    val debugStartCycle = testConfig.getInt("Debug Start Cycle").getOrElse(0)
    val debugEndCycle = testConfig.getInt("Debug End Cycle").getOrElse(0)

    //Hardware configuration
    val dataflow = testConfig.getString("Dataflow").get match {
      case "IS" => Dataflow.Is
      case "OS" => Dataflow.Os
      case "WS" => Dataflow.Ws
      case _ =>
        throw ParseError("Invalid dataflow")
    }

    val streamingDimensionSize = testConfig.
      getInt("Streaming Dimension Size").
      getOrElse(getMaximumStreamingDimension(gemmDimension, dataflow))

    val dramUploadOrder = dataflow match {
      case Dataflow.Is => DramUploadOrder.mkn
      case Dataflow.Os => DramUploadOrder.mnk
      case Dataflow.Ws => DramUploadOrder.knm
    }

    val groupPeRow = testConfig.getInt("Group PE Row").getOrElse(
      throw ParseError("Group PE Row not found")
    )
    val groupPeCol = testConfig.getInt("Group PE Column").getOrElse(
      throw ParseError("Group PE Col not found")
    )
    val vectorPeRow = testConfig.getInt("Vector PE Row").getOrElse(
      throw ParseError("Vector PE Row not found")
    )
    val vectorPeCol = testConfig.getInt("Vector PE Column").getOrElse(
      throw ParseError("Vector PE Column not found")
    )
    val numMultiplier = testConfig.getInt("Multipliers Per PE").getOrElse(
      throw ParseError("Multipliers Per PE not found")
    )

    val bitWidthPortA = testConfig.getInt("Port A Bit Width").getOrElse(
      throw ParseError("Port A Bit Width not found")
    )

    val bitWidthPortB = testConfig.getInt("Port B Bit Width").getOrElse(
      throw ParseError("Port B Bit Width not found")
    )

    val bitWidthPortC = calculateOutputPort(
      dataflow,
      groupPeRow = groupPeRow,
      groupPeCol = groupPeCol,
      vectorPeRow = vectorPeRow,
      vectorPeCol = vectorPeCol,
      numMultiplier = numMultiplier,
      bitWidthPortA = bitWidthPortA,
      bitWidthPortB = bitWidthPortB,
      streamingDimensionSize = streamingDimensionSize
    )

    val portBitWidth = PortBitWidth(bitWidthPortA, bitWidthPortB, bitWidthPortC)

    val dramBandwidth = testConfig.getInt("DRAM Bandwidth").getOrElse(
      throw ParseError("DRAM Bandwidth not found")
    )

    val singleBufferLimitKbA = testConfig.getInt("SRAM A Single Buffer Limit (KB)").getOrElse(
      throw ParseError("SRAM A Single Buffer Limit (KB)")
    )
    val singleBufferLimitKbB = testConfig.getInt("SRAM B Single Buffer Limit (KB)").getOrElse(
      throw ParseError("SRAM B Single Buffer Limit (KB)")
    )
    val singleBufferLimitKbC = testConfig.getInt("SRAM C Single Buffer Limit (KB)").getOrElse(
      throw ParseError("SRAM C Single Buffer Limit (KB)")
    )
    dramReferenceData.foreach(println(_))
    println(sramReferenceData)
    println(arrayReferenceData)

    SimulationConfig(
      debugPrint = debugPrint,
      debugStartCycle = debugStartCycle,
      debugEndCycle = debugEndCycle,
      layerName = layerName,
      layerGemmDimension = gemmDimension,
      streamingDimensionSize = streamingDimensionSize,
      dataflow = dataflow,
      groupPeRow = groupPeRow,
      groupPeCol = groupPeCol,
      vectorPeRow = vectorPeRow,
      vectorPeCol = vectorPeCol,
      numMultiplier = numMultiplier,
      portBitWidth = portBitWidth,
      dramBandwidth = dramBandwidth,
      dramUploadOrder = dramUploadOrder,
      singleBufferLimitKbA = singleBufferLimitKbA,
      singleBufferLimitKbB = singleBufferLimitKbB,
      singleBufferLimitKbC = singleBufferLimitKbC,
      sramReferenceDataVector = sramReferenceData,
      dramReferenceData = dramReferenceData,
      arrayReferenceData = arrayReferenceData,
    )

  }

  private def initializeComponents(simConfig: SimulationConfig) = {

    //Logger init
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

    val arrayDimension = new ArrayDimension(
      simConfig.groupPeRow,
      simConfig.groupPeCol,
      simConfig.vectorPeRow,
      simConfig.vectorPeCol,
      simConfig.numMultiplier,
    )

    val arrayConfig = ArrayConfig(
      arrayDimension = arrayDimension,
      dataflow = simConfig.dataflow,
      portBitWidth =  simConfig.portBitWidth,
    )

    val layer = new Layer(
      layerName = simConfig.layerName,
      gemmDimension = simConfig.layerGemmDimension,
      arrayConfig = arrayConfig,
      streamingDimensionSize = simConfig.streamingDimensionSize,
      dramUploadOrder = simConfig.dramUploadOrder,
      loggerOption = loggerOption
    )

    val dram = new Dram(
      outputBandwidth = simConfig.dramBandwidth,
      loggerOption = loggerOption
    )

    val sramComponents = buildSrams(
      arrayConfig = arrayConfig,
      simConfig = simConfig,
      sizeTileA = layer.operationVector.head.generateTileA.dims.memorySize,
      sizeTileB = layer.operationVector.head.generateTileB.dims.memorySize,
      sizeTileC = layer.operationVector.head.generateTileC.dims.memorySize,
      loggerOption = loggerOption
    )

    val array = new Array(
      arrayConfig = arrayConfig,
      loggerOption = loggerOption
    )

    val interface = new Interface(
      dram = dram,
      sramA = sramComponents._1,
      sramB = sramComponents._2,
      sramC = sramComponents._3,
      array = array
    )

    ((layer, array, interface, dram, sramComponents, loggerOption), loggerOption)

  }

  private def generateLogFileName(config: SimulationConfig): String = {

    val dataflowShortName = config.dataflow.toString.toLowerCase

    s"/debug_${config.layerName}_${dataflowShortName}_" +
      s"{${config.groupPeRow}x${config.groupPeCol}}x" +
      s"{${config.vectorPeRow}x${config.vectorPeCol}}" +
      s"x${config.numMultiplier}}" +
      s"_sd_size_${config.streamingDimensionSize}" +
      s"_buffer_A:${config.singleBufferLimitKbA}" +
      s"_buffer_B:${config.singleBufferLimitKbB}" +
      s"_buffer_C:${config.singleBufferLimitKbC}"

  }

  private def buildSrams(
    arrayConfig: ArrayConfig,
    simConfig: SimulationConfig,
    sizeTileA: Int,
    sizeTileB: Int,
    sizeTileC: Int,
    loggerOption: LoggerOption
  ) = {

    val capacityA = (simConfig.singleBufferLimitKbA * 8 * 1024)/ sizeTileA
    val capacityB = (simConfig.singleBufferLimitKbB * 8 * 1024)/ sizeTileB
    val capacityC = (simConfig.singleBufferLimitKbC * 8 * 1024)/ sizeTileC

//    println(s"Capacity A = ${capacityA}")
//    println(s"Capacity B = ${capacityB}")
//    println(s"Capacity C = ${capacityC}")


    if(!(capacityA > 0 && capacityB > 0 && capacityC > 0 ))
      throw RunTimeError("SRAM Cannot contain even 1 tile increase SRAM size")

    val sramA = new DoubleBufferSram(
      dataType = DataType.A,
      outputBandwidth = arrayConfig.bandwidthOfInputA,
      singleBufferTileCapacity = capacityA,
      singleBufferLimitKb = simConfig.singleBufferLimitKbA,
      loggerOption = loggerOption
    )
    val sramB = new DoubleBufferSram(
      dataType = DataType.B,
      outputBandwidth = arrayConfig.bandwidthOfInputB,
      singleBufferTileCapacity = capacityB,
      singleBufferLimitKb = simConfig.singleBufferLimitKbB,
      loggerOption = loggerOption
    )
    val sramC = new OutputDoubleBufferSram(
      outputBandwidth = simConfig.dramBandwidth,
      singleBufferTileCapacity = capacityC,
      singleBufferLimitKb = simConfig.singleBufferLimitKbC,
      loggerOption = loggerOption
    )

    (sramA, sramB, sramC)

  }

  private def runSimulation(
    simConfig: SimulationConfig,
    components: (Layer, Array, Interface, Dram, (DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram), LoggerOption)
  ): SimulationResult = {
    val (layer, array, interface, dram, srams, loggerOption) = components
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
      debugStartCycle = simConfig.debugStartCycle,
      debugEndCycle = simConfig.debugEndCycle,
      debugMode = simConfig.debugPrint,
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
        Console.err.println("Program Wrong")
        sys.exit(1)
    }

  }

  private def logSimulation(simConfig: SimulationConfig): Unit = {

    log("[Simulation Overview]]")
    log("\t[Target Layer]")
    log(s"\t\tLayer Name: ${simConfig.layerName}")
    log(s"\t\tM: ${simConfig.layerGemmDimension.m}, N: ${simConfig.layerGemmDimension.n}, K: ${simConfig.layerGemmDimension.k}")
    log(s"\t\tStreaming Dimension Size: ${simConfig.streamingDimensionSize}")
    log("")
    log(s"\t[DRAM]")
    log(s"\t\tDRAM Bandwidth: ${simConfig.dramBandwidth} bit")
    log("")
    log("\t[Input SRAM A]")
    log(s"\t\tSRAM A Single Buffer Limit (KB): ${simConfig.singleBufferLimitKbA} KB")
    log(s"\t\tSRAM A Single Buffer Limit (bit): ${simConfig.singleBufferLimitKbA * 8 * 1024} bit")
    log(s"\t\tSRAM A Total SRAM Size (KB): ${simConfig.singleBufferLimitKbA * 2} KB")
    log(s"\t\tSRAM A Total SRAM Size (bit): ${simConfig.singleBufferLimitKbA * 2 * 8 * 1024} bit")
    log("")
    log("\t[Input SRAM B]")
    log(s"\t\tSRAM B Single Buffer Limit (KB): ${simConfig.singleBufferLimitKbB} KB")
    log(s"\t\tSRAM B Single Buffer Limit (bit): ${simConfig.singleBufferLimitKbB * 8 * 1024} bit")
    log(s"\t\tSRAM B Total SRAM Size (KB): ${simConfig.singleBufferLimitKbB * 2} KB")
    log(s"\t\tSRAM B Total SRAM Size (bit): ${simConfig.singleBufferLimitKbB * 2 * 8 * 1024} bit")
    log("")
    log("\t[Output SRAM C]")
    log(s"\t\tSRAM B Single Buffer Limit (KB): ${simConfig.singleBufferLimitKbC} KB")
    log(s"\t\tSRAM B Single Buffer Limit (bit): ${simConfig.singleBufferLimitKbC * 8 * 1024} bit")
    log(s"\t\tSRAM B Total SRAM Size (KB): ${simConfig.singleBufferLimitKbC * 2} KB")
    log(s"\t\tSRAM B Total SRAM Size (bit): ${simConfig.singleBufferLimitKbC * 2 * 8 * 1024} bit")
    log("")
    log(s"\t[Systolic Tensor Array]")
    log(s"\t\t[Systolic Tensor Array Dimension]")
    logWithoutNewLine("\t\t\tDataflow: ")
    simConfig.dataflow match {
      case Dataflow.Is => log("Input Stationary")
      case Dataflow.Os => log("Output Stationary")
      case Dataflow.Ws => log("Weight Stationary")
      case _ =>
        Console.err.println(s"Invalid dataflow")
        sys.exit(1)
    }
    log(s"\t\t\tGroup PE Row: ${simConfig.groupPeRow}")
    log(s"\t\t\tGroup PE Block: ${simConfig.groupPeCol}")
    log(s"\t\t\tVector PE Row: ${simConfig.vectorPeRow}")
    log(s"\t\t\tVector PE Block: ${simConfig.vectorPeCol}")
    log(s"\t\t\tMultipliers Per PE: ${simConfig.numMultiplier}")
    log("")
    log(s"\t\t[Port Bit Width info]")
    log(s"\t\t\tPort A Bit Width: ${simConfig.portBitWidth.typeA}")
    log(s"\t\t\tPort B Bit Width: ${simConfig.portBitWidth.typeB}")
    log(s"\t\t\tPort C Bit Width: ${simConfig.portBitWidth.typeC}")
    log(s"")
    log(s"\t[Tile Size info]")
    log(s"\t\tStreaming Dimension Size: ${simConfig.streamingDimensionSize}")
    log(s"")

//    log(s"[SRAM Energy info]")
//    simConfig.sramReferenceDataVector.foreach{ energy =>
//      log(s"Capacity: ${energy.capacityKb} (KB)," +
//        s" Bandwidth: ${energy.bandwidthBytes} (Byte)," +
//        s" Read Energy: ${energy.readEnergyPj} (pJ)," +
//        s" Write energy: ${energy.writeEnergyPj} (pJ)," +
//        s" Leakage Power ${energy.leakagePowerMw} (mW)")
//    }




  }

}

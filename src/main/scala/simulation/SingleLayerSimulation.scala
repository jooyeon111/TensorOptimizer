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
//    arrayReferenceData: Option[ArrayReferenceData],
//    sramReferenceDataVector: Option[Vector[SramReferenceData]],
//    dramReferenceData: Option[DramReferenceData],
  ) {
    def validate: Boolean = {
      streamingDimensionSize > 0 &&
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
      //TODO add validation code
//        arrayReferenceData.validate &&
//        sramReferenceDataVector.forall( x => x.validate) &&
//        dramReferenceData.validate
    }
  }

  def processOneLayer(
    layerPath: String,
    testPath: String,
//    sramDataPath: String,
//    dramDataPath: String,
//    arrayDataPath: String,
    help: String
  ): Unit = {

    val layerConfigParser = new ConfigManager(layerPath)
    val testConfigParser = new ConfigManager(testPath)
//    val sramDataParser = new ConfigManager(sramDataPath)
//    val dramDataParser = new ConfigManager(dramDataPath)
//    val arrayDataParser = new ConfigManager(arrayDataPath)

    if(!layerConfigParser.parse()) {
      throw ParseError("Layer parsing failed" + help)
    }

    if(!testConfigParser.parse()){
      throw ParseError("Test option parsing failed" + help)
    }

//    if(!sramDataParser.parse()){
//      throw ParseError("SRAM Energy configuration parsing failed" + help)
//    }
//
//    if(!dramDataParser.parse()){
//      throw ParseError("DRAM Energy configuration parsing failed" + help)
//    }
//
//    if(!arrayDataParser.parse()){
//      throw ParseError("DRAM Energy configuration parsing failed" + help)
//    }

    val layerConfig = layerConfigParser.getConfig.getOrElse(
      throw ParseError("Layer config not found")
    )
    val testConfig = testConfigParser.getConfig.getOrElse(
      throw ParseError("Test Config not found")
    )

//    val sramDataConfig = sramDataParser.getConfig.getOrElse(
//      throw ParseError("SRAM config not found")
//    )
//
//    val dramDataConfig = dramDataParser.getConfig.getOrElse(
//      throw ParseError("DRAM config not found")
//    )
//
//    val arrayDataConfig = arrayDataParser.getConfig.getOrElse(
//      throw ParseError("Array config not found")
//    )

    val simulationConfig = buildSimulationOneLayerConfig(
      layerConfig = layerConfig,
      testConfig = testConfig,
//      sramDataVector = sramDataConfig.sramReferenceDataVector,
//      dramDataConfig = dramDataConfig,
//      arrayDataConfig = arrayDataConfig
    )

    if(!simulationConfig.validate)
      throw ParseError("Invalid simulation configuration")

    println("Initialize Components")
    val (components, loggerOption) = initializeComponents(simulationConfig)

    logSimulation(simulationConfig)

    println("Run Simulation")
    val simResult = runSimulation(simulationConfig, components)

//  TODO change it after few simulations
    simResult.printFullResults(loggerOption)
//    simResult.showSummary(loggerOption)

  }

  private def buildSimulationOneLayerConfig(
    layerConfig: ConfigParser.Config,
    testConfig: ConfigParser.Config,
//    sramDataVector: Vector[SramReferenceData],
//    dramDataConfig: ConfigParser.Config,
//    arrayDataConfig: ConfigParser.Config
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

//    val leakagePowerGroupPeRow: Double = arrayDataConfig.getDouble("Leakage Power Group PE Row").getOrElse(
//      throw ParseError("Array Data Not found")
//    )
//
//    val leakagePowerGroupPeCol: Double = arrayDataConfig.getDouble("Leakage Power Group PE Column").getOrElse(
//      throw ParseError("Array Data Not found")
//    )
//    val leakagePowerVectorPeRow: Double = arrayDataConfig.getDouble("Leakage Power Vector PE Row").getOrElse(
//      throw ParseError("Array Data Not found")
//    )
//    val leakagePowerVectorPeCol: Double = arrayDataConfig.getDouble("Leakage Power Vector PE Column").getOrElse(
//      throw ParseError("Array Data Not found")
//    )
//    val leakagePowerNumMultiplier: Double = arrayDataConfig.getDouble("Leakage Power Total Number of Multipliers").getOrElse(
//      throw ParseError("Array Data Not found")
//    )
//    val dynamicPowerGroupPeRow: Double = arrayDataConfig.getDouble("Dynamic Power Group PE Row").getOrElse(
//      throw ParseError("Array Data Not found")
//    )
//    val dynamicPowerGroupPeCol: Double = arrayDataConfig.getDouble("Dynamic Power Group PE Column").getOrElse(
//      throw ParseError("Array Data Not found")
//    )
//    val dynamicPowerVectorPeRow: Double = arrayDataConfig.getDouble("Dynamic Power Vector PE Row").getOrElse(
//      throw ParseError("Array Data Not found")
//    )
//    val dynamicPowerVectorPeCol: Double = arrayDataConfig.getDouble("Dynamic Power Vector PE Column").getOrElse(
//      throw ParseError("Array Data Not found")
//    )
//    val dynamicPowerNumMultiplier: Double = arrayDataConfig.getDouble("Dynamic Power Total Number of Multipliers").getOrElse(
//      throw ParseError("Array Data Not found")
//    )
//    val areaPowerGroupPeRow: Double = arrayDataConfig.getDouble("Area Group PE Row").getOrElse(
//      throw ParseError("Array Data Not found")
//    )
//    val areaPowerGroupPeCol: Double = arrayDataConfig.getDouble("Area Group PE Column").getOrElse(
//      throw ParseError("Array Data Not found")
//    )
//    val areaPowerVectorPeRow: Double = arrayDataConfig.getDouble("Area Vector PE Row").getOrElse(
//      throw ParseError("Array Data Not found")
//    )
//    val areaPowerVectorPeCol: Double = arrayDataConfig.getDouble("Area Vector PE Column").getOrElse(
//      throw ParseError("Array Data Not found")
//    )
//    val areaPowerNumMultiplier: Double = arrayDataConfig.getDouble("Area Total Number of Multipliers").getOrElse(
//      throw ParseError("Array Data Not found")
//    )
//
//    val arrayDataReference = ArrayReferenceData(
//      leakagePowerGroupPeRowMw = leakagePowerGroupPeRow,
//      leakagePowerGroupPeColMw = leakagePowerGroupPeCol,
//      leakagePowerVectorPeRowMw = leakagePowerVectorPeRow,
//      leakagePowerVectorPeColMw = leakagePowerVectorPeCol,
//      leakagePowerNumMultiplierMw = leakagePowerNumMultiplier,
//      dynamicPowerGroupPeRowPj = dynamicPowerGroupPeRow,
//      dynamicPowerGroupPeColPj = dynamicPowerGroupPeCol,
//      dynamicPowerVectorPeRowPj = dynamicPowerVectorPeRow,
//      dynamicPowerVectorPeColPj = dynamicPowerVectorPeCol,
//      dynamicPowerNumMultiplierPj = dynamicPowerNumMultiplier,
//      areaPowerGroupPeRowUm2 = areaPowerGroupPeRow,
//      areaPowerGroupPeColUm2 = areaPowerGroupPeCol,
//      areaPowerVectorPeRowUm2 = areaPowerVectorPeRow,
//      areaPowerVectorPeColUm2 = areaPowerVectorPeCol,
//      areaPowerNumMultiplierUm2 = areaPowerNumMultiplier,
//    )
//
//    val dramReadEnergy: Double = dramDataConfig.getDouble("Read Energy").getOrElse(
//      throw ParseError("DRAM Data Not found")
//    )
//
//    val dramWriteEnergy: Double = dramDataConfig.getDouble("Write Energy").getOrElse(
//      throw ParseError("DRAM Data Not found")
//    )
//
//    val dramReferenceData: DramReferenceData = DramReferenceData(
//      readEnergyPj = dramReadEnergy,
//      writeEnergyPj = dramWriteEnergy,
//    )

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
//      sramReferenceDataVector = sramDataVector,
//      dramReferenceData = dramReferenceData,
//      arrayReferenceData = arrayDataReference,
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

    println(s"Capacity A = ${capacityA}")
    println(s"Capacity B = ${capacityB}")
    println(s"Capacity C = ${capacityC}")


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

    val compiler = new Compiler(
      dram = dram,
      sramA = sramA,
      sramB = sramB,
      sramC = sramC,
      interface = interface,
      layer = layer,
      array = array,
//      sramDataReferenceVector = simConfig.sramReferenceDataVector,
//      dramReferenceDataData = simConfig.dramReferenceData,
//      arrayReferenceData = simConfig.arrayReferenceData,
      debugStartCycle = simConfig.debugStartCycle,
      debugEndCycle = simConfig.debugEndCycle,
      debugMode = simConfig.debugPrint,
      loggerOption = loggerOption,
    )

    try {

      compiler.run()

      SimulationResult(
        totalOperationNumber = compiler.getTotalOperationNumber,
        tileSizeA = compiler.getTileSizeA,
        tileSizeB = compiler.getTileSizeB,
        tileSizeC = compiler.getTileSizeC,
        trimTileCountA = compiler.getTrimTileCountA,
        trimTileCountB = compiler.getTrimTileCountB,
        singleBufferTileCapacityA = compiler.getSingleBufferTileCapacityA,
        singleBufferTileCapacityB = compiler.getSingleBufferTileCapacityB,
        singleBufferTileCapacityC = compiler.getSingleBufferTileCapacityC,

        arrayInputBandwidthA = compiler.getArrayInputBandwidthA,
        arrayInputBandwidthB = compiler.getArrayInputBandwidthB,
        arrayOutputBandwidthC = compiler.getArrayOutputBandwidthC,

        arrayCapacityA = compiler.getArrayCapacityA,
        arrayCapacityB = compiler.getArrayCapacityB,

        cycle = compiler.getTotalCycle,
        arrayActiveCount = compiler.getArrayActiveCount,


        dramLogs = compiler.getDramLogs,
        dramReadAccessCount = compiler.getDramReadAccessCount,
        dramWriteAccessCount = compiler.getDramWriteAccessCount,
        sramReadAccessCountA = compiler.getSramReadAccessCountA,
        sramWriteAccessCountA = compiler.getSramWriteAccessCountA,
        sramReadAccessCountB = compiler.getSramReadAccessCountB,
        sramWriteAccessCountB = compiler.getSramWriteAccessCountB,

        dramHitRatio = compiler.getTotalDramHitCount,
        dramMissRatio = compiler.getTotalDramMissCount,

        sramHitRatioA = compiler.getSramHitRatioA,
        sramHitRatioB = compiler.getSramHitRatioB,
        sramMissRatioA = compiler.getSramMissRatioA,
        sramMissRatioB = compiler.getSramMissRatioB,
        sramHitRatio = compiler.getTotalSramHitRatio,
        sramMissRatio = compiler.getTotalSramMissRatio,

        dramStallCount = compiler.getDramStallCount,

        firstFillUpCycleA = compiler.getFirstFillUptCycleA,
        bufferSwapCountA = compiler.getBufferSwapCountA,
        bufferSwapStallCountA = compiler.getBufferSwapStallCountA,

        firstFillUpCycleB = compiler.getFirstFillUptCycleB,
        bufferSwapCountB = compiler.getBufferSwapCountB,
        bufferSwapStallCountB = compiler.getBufferSwapStallCountB,

        arrayInputStallCount = compiler.getArrayInputStallCount,
        arrayOutputStallCount = compiler.getArrayOutputStallCount,

        firstFillUpCycleC = compiler.getFirstFillUptCycleC,
        bufferSwapCountC = compiler.getBufferSwapCountC,
        bufferSwapStallCountC = compiler.getBufferSwapStallCountC,

        averageMemoryUsageKbA = compiler.getAverageMemoryUsageKbA,
        averageMemoryUtilizationA = compiler.getAverageMemoryUtilizationA,
        averageMemoryUsageKbB = compiler.getAverageMemoryUsageKbB,
        averageMemoryUtilizationB = compiler.getAverageMemoryUtilizationB,
        averageMemoryUsageKbC = compiler.getAverageMemoryUsageKbC,
        averageMemoryUtilizationC = compiler.getAverageMemoryUtilizationC,

//        sramAreaMmA = compiler.getSramAreaMmA,
//        sramAreaMmB = compiler.getSramAreaMmB,
//        sramAreaMmC = compiler.getSramAreaMmC,
//        dramAreaMm = compiler.getDramAreaMm,
//        arrayAreaMm = compiler.getArrayAreaMm,
//        areaMm = compiler.getTotalArea,
//
//        sramReadEnergyPjA = compiler.getSramReadEnergyA,
//        sramWriteEnergyPjA = compiler.getSramWriteEnergyA,
//        sramLeakageEnergyPjA = compiler.getSramLeakageEnergyA,
//        sramEnergyPjA = compiler.getSramEnergyA,
//
//        sramReadEnergyPjB = compiler.getSramWriteEnergyB,
//        sramWriteEnergyPjB = compiler.getSramWriteEnergyB,
//        sramLeakageEnergyPjB = compiler.getSramLeakageEnergyB,
//        sramEnergyPjB = compiler.getSramEnergyB,
//
//        sramReadEnergyPjC = compiler.getSramReadEnergyC,
//        sramWriteEnergyPjC = compiler.getSramWriteEnergyC,
//        sramLeakageEnergyPjC = compiler.getSramLeakageEnergyC,
//        sramEnergyPjC = compiler.getSramEnergyC,
//
//        dramReadEnergyPj = compiler.getDramReadEnergy,
//        dramWriteEnergyPj = compiler.getDramWriteEnergy,
//        dramEnergyPj = compiler.getDramEnergy,
//
//        arrayDynamicEnergyPj = compiler.getArrayDynamicEnergy,
//        arrayLeakageEnergyPj = compiler.getArrayLeakageEnergy,
//        arrayEnergy = compiler.getArrayEnergy,
//
//        energyPj = compiler.getTotalEnergy,
      )

    } catch {
      case _: RunTimeError =>
        SimulationResult(Long.MaxValue)
    }

  }

  private def logSimulation(simConfig: SimulationConfig): Unit = {

    log("Cycle Simulation Per Systolic Tensor Configuration")
    log("")
    log("[Target Layer]")
    log(s"\tLayer Name: ${simConfig.layerName}")
    log(s"\tM: ${simConfig.layerGemmDimension.m}, N: ${simConfig.layerGemmDimension.n}, K: ${simConfig.layerGemmDimension.k}")
    log(s"\tStreaming Dimension Size: ${simConfig.streamingDimensionSize}")
    log("")
    log(s"[DRAM]")
    log(s"\tDRAM Bandwidth: ${simConfig.dramBandwidth} bit")
    log("")
    log("[Input SRAM A]")
    log(s"\tSRAM A Single Buffer Limit (KB): ${simConfig.singleBufferLimitKbA} KB")
    log(s"\tSRAM A Single Buffer Limit (bit): ${simConfig.singleBufferLimitKbA * 8 * 1024} bit")
    log(s"\tSRAM A Total SRAM Size (KB): ${simConfig.singleBufferLimitKbA * 2} KB")
    log(s"\tSRAM A Total SRAM Size (bit): ${simConfig.singleBufferLimitKbA * 2 * 8 * 1024} bit")
    log("")
    log("[Input SRAM B]")
    log(s"\tSRAM B Single Buffer Limit (KB): ${simConfig.singleBufferLimitKbB} KB")
    log(s"\tSRAM B Single Buffer Limit (bit): ${simConfig.singleBufferLimitKbB * 8 * 1024} bit")
    log(s"\tSRAM B Total SRAM Size (KB): ${simConfig.singleBufferLimitKbB * 2} KB")
    log(s"\tSRAM B Total SRAM Size (bit): ${simConfig.singleBufferLimitKbB * 2 * 8 * 1024} bit")
    log("")
    log("[Output SRAM C]")
    log(s"\tSRAM B Single Buffer Limit (KB): ${simConfig.singleBufferLimitKbC} KB")
    log(s"\tSRAM B Single Buffer Limit (bit): ${simConfig.singleBufferLimitKbC * 8 * 1024} bit")
    log(s"\tSRAM B Total SRAM Size (KB): ${simConfig.singleBufferLimitKbC * 2} KB")
    log(s"\tSRAM B Total SRAM Size (bit): ${simConfig.singleBufferLimitKbC * 2 * 8 * 1024} bit")
    log("")
    log(s"[Systolic Tensor Array]")
    log(s"\t[Systolic Tensor Array Dimension]")
    logWithoutNewLine("\t\tDataflow: ")
    simConfig.dataflow match {
      case Dataflow.Is => log("Input Stationary")
      case Dataflow.Os => log("Output Stationary")
      case Dataflow.Ws => log("Weight Stationary")
      case _ =>
        Console.err.println(s"Invalid dataflow")
        sys.exit(1)
    }
    log(s"\t\tGroup PE Row: ${simConfig.groupPeRow}")
    log(s"\t\tGroup PE Block: ${simConfig.groupPeCol}")
    log(s"\t\tVector PE Row: ${simConfig.vectorPeRow}")
    log(s"\t\tVector PE Block: ${simConfig.vectorPeCol}")
    log(s"\t\tMultipliers Per PE: ${simConfig.numMultiplier}")
    log("")
    log(s"\t[Port Bit Width info]")
    log(s"\t\tPort A Bit Width: ${simConfig.portBitWidth.typeA}")
    log(s"\t\tPort B Bit Width: ${simConfig.portBitWidth.typeB}")
    log(s"\t\tPort C Bit Width: ${simConfig.portBitWidth.typeC}")
    log(s"")
    log(s"[Tile Size info]")

//    log(s"[SRAM Energy info]")
//    simConfig.sramReferenceDataVector.foreach{ energy =>
//      log(s"Capacity: ${energy.capacityKb} (KB)," +
//        s" Bandwidth: ${energy.bandwidthBytes} (Byte)," +
//        s" Read Energy: ${energy.readEnergyPj} (pJ)," +
//        s" Write energy: ${energy.writeEnergyPj} (pJ)," +
//        s" Leakage Power ${energy.leakagePowerMw} (mW)")
//    }

    log(s"\tStreaming Dimension Size: ${simConfig.streamingDimensionSize}")
    log(s"")


  }

}

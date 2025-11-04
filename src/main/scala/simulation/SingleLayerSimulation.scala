package simulation

import java.io.File
import common.{ArrayDimension, Dataflow, FilePaths, OutputPortCalculator}
import scala.util.{Failure, Success}

trait SingleLayerSimulation extends OutputPortCalculator with Logger {

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
    offChipMemoryBandwidth: Int,
    offChipMemoryUploadOrder: OffChipMemoryUploadOrder.Value,
    singleBufferLimitKbA: Int,
    singleBufferLimitKbB: Int,
    singleBufferLimitKbC: Int,
    offChipMemoryReferenceData: Option[OffChipMemoryReferenceData],
    sramReferenceDataVector: Option[Vector[SramReferenceData]],
    arraySynthesisSource: Option[ArraySynthesisSource.Value],
    arraySynthesisData: Option[ArraySynthesisData],
    forcedSramAreaA: Option[Double],
    forcedSramAreaB: Option[Double],
    forcedSramAreaC: Option[Double],
    prefix: Option[String],
    suffix: Option[String],
  ) {
    def validate: Boolean = {
      val baseValidation = streamingDimensionSize > 0 &&
        groupPeRow > 0 &&
        groupPeCol > 0 &&
        vectorPeRow > 0 &&
        vectorPeCol > 0 &&
        numMultiplier > 0 &&
        offChipMemoryBandwidth > 0 &&
        singleBufferLimitKbA > 0 &&
        singleBufferLimitKbB > 0 &&
        singleBufferLimitKbC > 0 &&
        portBitWidth.validate &&
        layerGemmDimension.validate

      val energyValidation = (offChipMemoryReferenceData, sramReferenceDataVector, arraySynthesisData) match {
        case (Some(offChipMemoryReferenceData), Some(sram), Some(array)) =>
          offChipMemoryReferenceData.validate && sram.forall(_.validate) && array.validate
        case (None, None, None) =>
          true
        case _ =>
          false
      }

      baseValidation && energyValidation

    }
  }

  //TODO reorder parameters
  def runLayerSimulation(
    layerPath: String,
    testPath: String,
    tilingPath: String,
    offChipMemoryDataPath: Option[String] = None,
    sramDataPath: Option[String] = None,
    arrayDataPath: Option[String] = None,
    help: String
  ): Unit = {

    val layerConfigParser = new ConfigManager(layerPath)
    val testConfigParser = new ConfigManager(testPath)
    val tilingConfigParser = new ConfigManager(tilingPath)

    val sramDataParser = sramDataPath.map(new ConfigManager(_))
    val offChipMemoryDataParser = offChipMemoryDataPath.map(new ConfigManager(_))
    val arrayDataParser = arrayDataPath.map(new ConfigManager(_))

    if(!layerConfigParser.parse()) {
      throw ParseError("Layer parsing failed" + help)
    }

    if(!testConfigParser.parse()){
      throw ParseError("Test option parsing failed" + help)
    }

    if(!tilingConfigParser.parse()){
      throw ParseError("Tiling option parsing failed" + help)
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

    arrayDataParser.foreach(parser =>
      if(!parser.parse()){
        throw ParseError("Array Synthesis dat parsing failed" + help)
      }
    )

    val layerConfig = layerConfigParser.getConfig.getOrElse(
      throw ParseError("Layer config not found")
    )
    val testConfig = testConfigParser.getConfig.getOrElse(
      throw ParseError("Test Config not found")
    )

    val tilingConfig = tilingConfigParser.getConfig.getOrElse(
      throw ParseError("Tiling Config not found")
    )

    val offChipMemoryReferenceData = offChipMemoryDataParser.flatMap(_.getConfig).map { config =>
      OffChipMemoryReferenceData(
        readEnergyPj = config.getDouble("Read Energy").getOrElse(
          throw ParseError("offChipMemory Read Energy Not found")
        ),
        writeEnergyPj = config.getDouble("Write Energy").getOrElse(
          throw ParseError("offChipMemory Write Energy Not found")
        )
      )
    }

    val sramReferenceData = sramDataParser.flatMap(_.getConfig).map(_.sramReferenceDataVector)

    val arraySynthesisSource: Option[ArraySynthesisSource.Value] = if(arrayDataParser.isDefined){
      Some(ArraySynthesisSource.DesignCompiler)
    } else {
      Some(ArraySynthesisSource.FewShotPrediction)
    }

    val arraySynthesisData = arrayDataParser.flatMap(_.getConfig).map { config =>
      val result = ArraySynthesisData(
        areaUm2 = config.getDouble("Area").getOrElse(
          throw ParseError("Array Area not found")
        ),
        switchPowerMw = config.getDouble("Switch Power").getOrElse(
          throw ParseError("Switch Power Not found")
        ),
        internalPowerMw = config.getDouble("Internal Power").getOrElse(
          throw ParseError("Internal Power Not found")
        ),
        leakagePowerMw = config.getDouble("Leakage Power").getOrElse(
          throw ParseError("Leakage Power Not found")
        )
      )
      result
    }

    println("Build Simulation Config")
    val simulationConfig = buildSimulationOneLayerConfig(
      layerConfig = layerConfig,
      testConfig = testConfig,
      tilingConfig = tilingConfig,
      offChipMemoryReferenceData = offChipMemoryReferenceData,
      sramReferenceData = sramReferenceData,
      arraySynthesisSource = arraySynthesisSource,
      arraySynthesisData = arraySynthesisData
    )

    if(!simulationConfig.validate) {
//      simulationConfig.printAllConfigurationValues()
      throw ParseError("Invalid simulation configuration")
    }

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
    tilingConfig: ConfigParser.Config,
    offChipMemoryReferenceData: Option[OffChipMemoryReferenceData] = None,
    sramReferenceData: Option[Vector[SramReferenceData]] = None,
    arraySynthesisSource: Option[ArraySynthesisSource.Value] = None,
    arraySynthesisData: Option[ArraySynthesisData] = None
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
    val dataflowString = testConfig.getString("Dataflow").get
    val dataflow = dataflowString match {
      case "IS" => Dataflow.Is
      case "OS" => Dataflow.Os
      case "WS" => Dataflow.Ws
      case _ =>
        throw ParseError("Invalid dataflow")
    }

    val streamingDimensionSize = parseStreamingDimension(
      tilingConfig, dataflow, dimensionM, dimensionN, dimensionK
    )

    val offChipMemoryUploadOrder = dataflow match {
      case Dataflow.Is => OffChipMemoryUploadOrder.mkn
      case Dataflow.Os => OffChipMemoryUploadOrder.mnk
      case Dataflow.Ws => OffChipMemoryUploadOrder.knm
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

    val offChipMemoryBandwidth = testConfig.getInt("Off Chip Memory Bandwidth").getOrElse(
      throw ParseError("Off Chip Memory Bandwidth not found")
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

    val forcedSramAreaA: Option[Double] = testConfig.getDouble("SRAM A Forced Area (um^2)")
    val forcedSramAreaB: Option[Double] = testConfig.getDouble("SRAM B Forced Area (um^2)")
    val forcedSramAreaC: Option[Double] = testConfig.getDouble("SRAM C Forced Area (um^2)")

    val prefix: Option[String] = testConfig.getString("prefix")
    val suffix: Option[String] = testConfig.getString("suffix")

    val arrayData: Option[ArraySynthesisData] = if(arraySynthesisData.isDefined) {
      println("Using design compiler array synthesis data")
      assert(arraySynthesisSource.get == ArraySynthesisSource.DesignCompiler,
        " This results not coming from design compiler")
      arraySynthesisData
    } else {
      assert(arraySynthesisSource.get == ArraySynthesisSource.FewShotPrediction,
        "This results not coming from prediction")
      if (!FewShotPredictor.isModelLoaded) {
        println("Loading ML prediction model...")
        FewShotPredictor.loadModelFromDefaultFiles match {
          case Success(_) =>
            println("Model loaded successfully")
          case Failure(e) =>
            println(s"Failed to load model: ${e.getMessage}")
            throw new RuntimeException(s"Could not load prediction model: ${e.getMessage}")
        }
      }

      FewShotPredictor.predict(
        FewShotPredictor.InputFeatures(
          dataflow = dataflowString,
          totalNumberOfMultipliers = groupPeRow * groupPeCol * vectorPeRow * vectorPeCol * numMultiplier,
          r = groupPeRow,
          c = groupPeCol,
          a = vectorPeRow,
          b = vectorPeCol,
          p = numMultiplier,
          streamingDimensionSize = streamingDimensionSize
        )
      ) match {
        case Success(result) =>

          println(s"Area: ${result.areaUm2}")
          println(s"Switch Power: ${result.switchPowerMw}")
          println(s"internal Power: ${result.internalPowerMw}")
          println(s"leakage Power: ${result.leakagePowerMw}")

          println("Synthesis Data Prediction Success")
          Some(result)
        case Failure(exception) =>
          println(s"Prediction failed: ${exception.getMessage}")
          throw new RuntimeException(s"Hardware prediction failed: ${exception.getMessage}")
      }

    }

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
      offChipMemoryBandwidth = offChipMemoryBandwidth,
      offChipMemoryUploadOrder = offChipMemoryUploadOrder,
      singleBufferLimitKbA = singleBufferLimitKbA,
      singleBufferLimitKbB = singleBufferLimitKbB,
      singleBufferLimitKbC = singleBufferLimitKbC,
      sramReferenceDataVector = sramReferenceData,
      offChipMemoryReferenceData = offChipMemoryReferenceData,
      arraySynthesisSource = arraySynthesisSource,
      arraySynthesisData = arrayData,
      forcedSramAreaA = forcedSramAreaA,
      forcedSramAreaB = forcedSramAreaB,
      forcedSramAreaC = forcedSramAreaC,
      prefix = prefix,
      suffix = suffix,
    )

  }

  private def parseStreamingDimension(
    tilingConfig: ConfigParser.Config,
    dataflow: Dataflow.Value,
    dimensionM: Int,
    dimensionN: Int,
    dimensionK: Int
  ): Int = {

    val configuredStreamingDim = tilingConfig
      .getInt("Streaming Dimension Size")
      .getOrElse(throw ParseError("Streaming Dimension Not Found"))

    val limitDimension = dataflow match {
      case Dataflow.Is => dimensionN
      case Dataflow.Os => dimensionK
      case Dataflow.Ws => dimensionM
      case _ => throw new RuntimeException(s"Invalid dataflow")
    }

    if (configuredStreamingDim >= limitDimension) {
      println("Streaming Dimension is Equal or Larger in output stationary")
      println(s"Streaming dimension in configuration file is $configuredStreamingDim")
      println(s"Streaming dimension is changed into $limitDimension")
      limitDimension
    } else {
      configuredStreamingDim
    }
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
      arraySynthesisData = simConfig.arraySynthesisData,
      arraySynthesisSource = simConfig.arraySynthesisSource,
    )

    val layer = new Layer(
      layerName = simConfig.layerName,
      gemmDimension = simConfig.layerGemmDimension,
      arrayConfig = arrayConfig,
      streamingDimensionSize = simConfig.streamingDimensionSize,
      offChipMemoryUploadOrder = simConfig.offChipMemoryUploadOrder,
      loggerOption = loggerOption
    )

    val sampleTileA = layer.operationVector.head.generateTileA
    val sampleTileB = layer.operationVector.head.generateTileB
    val sampleTileC = layer.operationVector.head.generateTileC

    val offChipMemory = new OffChipMemory(
      outputBandwidth = simConfig.offChipMemoryBandwidth,
      referenceData = simConfig.offChipMemoryReferenceData,
      loggerOption = loggerOption
    )

    val srams = buildSrams(
      arrayConfig = arrayConfig,
      simConfig = simConfig,
      tileSizeA = sampleTileA.dims.memorySize,
      tileSizeB = sampleTileB.dims.memorySize,
      tileSizeC = sampleTileC.dims.memorySize,
      loggerOption = loggerOption
    )

    val array = new Array(
      arrayConfig = arrayConfig,
      loggerOption = loggerOption
    )

    val interface = new Interface(
      offChipMemory = offChipMemory,
      sramA = srams._1,
      sramB = srams._2,
      sramC = srams._3,
      array = array
    )

    ((layer, array, interface, offChipMemory, srams, loggerOption), loggerOption)

  }

  private def generateLogFileName(config: SimulationConfig): String = {

    val dataflowShortName = config.dataflow.toString.toLowerCase
    val synthesisSource = config.arraySynthesisSource.get match {
      case ArraySynthesisSource.DesignCompiler =>
        "dc"
      case ArraySynthesisSource.FewShotPrediction =>
        "sim"
      case _ =>
        throw ParseError("Invalid synthesis source")
    }

    val baseName = s"/${synthesisSource}_${config.layerName}_${dataflowShortName}_" +
      s"{${config.groupPeRow}x${config.groupPeCol}}x" +
      s"{${config.vectorPeRow}x${config.vectorPeCol}}" +
      s"x${config.numMultiplier}}" +
      s"_sd_size_${config.streamingDimensionSize}" +
      s"_buffer_A:${config.singleBufferLimitKbA}" +
      s"_buffer_B:${config.singleBufferLimitKbB}" +
      s"_buffer_C:${config.singleBufferLimitKbC}"

//    s"${prefix.getOrElse("")}$baseName${suffix.getOrElse("")}"

    val prefixPart = config.prefix.map(_ + "_").getOrElse("")
    val suffixPart = config.suffix.map("_" + _).getOrElse("")

    s"/$prefixPart$baseName$suffixPart"

  }

  private def buildSrams(
    arrayConfig: ArrayConfig,
    simConfig: SimulationConfig,
    tileSizeA: Int,
    tileSizeB: Int,
    tileSizeC: Int,
    loggerOption: LoggerOption
  ) = {

    require(tileSizeA > 0, "[error] Tile is the negative value")
    require(tileSizeB > 0, "[error] Tile is the negative value")
    require(tileSizeC > 0, "[error] Tile is the negative value")

    val capacityA = (simConfig.singleBufferLimitKbA * 8 * 1024)/ tileSizeA
    val capacityB = (simConfig.singleBufferLimitKbB * 8 * 1024)/ tileSizeB
    val capacityC = (simConfig.singleBufferLimitKbC * 8 * 1024)/ tileSizeC

    if(!(capacityA > 0 && capacityB > 0 && capacityC > 0 ))
      throw SramBuildError("SRAM Cannot contain even 1 tile increase SRAM size")

    val sramReferenceDataA: Option[SramReferenceData] = simConfig.sramReferenceDataVector.flatMap{ vector =>
      vector.find{ data =>
        data.capacityKb == simConfig.singleBufferLimitKbA && data.bandwidthBits >= arrayConfig.bandwidthOfInputA
      }
    }

    val sramReferenceDataB: Option[SramReferenceData] = simConfig.sramReferenceDataVector.flatMap{ vector =>
      vector.find{ data =>
        data.capacityKb == simConfig.singleBufferLimitKbB && data.bandwidthBits >= arrayConfig.bandwidthOfInputB
      }
    }

    val sramReferenceDataC: Option[SramReferenceData] = simConfig.sramReferenceDataVector.flatMap{ vector =>
      vector.find{ data =>
        data.capacityKb == simConfig.singleBufferLimitKbC && data.bandwidthBits >= arrayConfig.outputBandwidth
      }
    }

    if(sramReferenceDataA.isEmpty || sramReferenceDataB.isEmpty || sramReferenceDataC.isEmpty){

      if(sramReferenceDataA.isEmpty)
        println("SRAM A data is empty")

      if(sramReferenceDataB.isEmpty)
        println("SRAM B data is empty")

      if(sramReferenceDataC.isEmpty)
        println("SRAM C data is empty")

      throw ParseError("Cannot find SRAM data from external reports ...")
    }

//    val sramRefDataA = if(simConfig.forcedSramAreaA.isDefined){
//      Some(sramReferenceDataA.get.copy(areaUm2 = simConfig.forcedSramAreaA.get))
//    } else {
//      sramReferenceDataA
//    }

    val sramRefDataA = sramReferenceDataA.map { data =>
      data.copy(areaUm2 = simConfig.forcedSramAreaA.getOrElse(data.areaUm2))
    }

//    val sramRefDataB = if(simConfig.forcedSramAreaB.isDefined){
//      Some(sramReferenceDataB.get.copy(areaUm2 = simConfig.forcedSramAreaB.get))
//    } else {
//      sramReferenceDataB
//    }

    val sramRefDataB = sramReferenceDataB.map { data =>
      data.copy(areaUm2 = simConfig.forcedSramAreaB.getOrElse(data.areaUm2))
    }

//    val sramRefDataC = if(simConfig.forcedSramAreaC.isDefined){
//      Some(sramReferenceDataC.get.copy(areaUm2 = simConfig.forcedSramAreaC.get))
//    } else {
//      sramReferenceDataC
//    }

    val sramRefDataC = sramReferenceDataC.map { data =>
      data.copy(areaUm2 = simConfig.forcedSramAreaC.getOrElse(data.areaUm2))
    }

    val sramA = new DoubleBufferSram(
      dataType = DataType.A,
      outputBandwidth = arrayConfig.bandwidthOfInputA,
      singleBufferTileCapacity = capacityA,
      singleBufferLimitKb = simConfig.singleBufferLimitKbA,
//      referenceData = sramReferenceDataA,
      referenceData = sramRefDataA,
      loggerOption = loggerOption
    )
    val sramB = new DoubleBufferSram(
      dataType = DataType.B,
      outputBandwidth = arrayConfig.bandwidthOfInputB,
      singleBufferTileCapacity = capacityB,
      singleBufferLimitKb = simConfig.singleBufferLimitKbB,
//      referenceData = sramReferenceDataB,
      referenceData = sramRefDataB,
      loggerOption = loggerOption
    )

    val sramC = new OutputDoubleBufferSram(
      outputBandwidth = simConfig.offChipMemoryBandwidth,
      singleBufferTileCapacity = capacityC,
      singleBufferLimitKb = simConfig.singleBufferLimitKbC,
//      referenceData = sramReferenceDataC,
      referenceData = sramRefDataC,
      loggerOption = loggerOption
    )

    (sramA, sramB, sramC)

  }

  private def runSimulation(
    simConfig: SimulationConfig,
    components: (Layer, Array, Interface, OffChipMemory, (DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram), LoggerOption)
  ): SimulationResult = {

    val (layer, array, interface, offChipMemory, srams, loggerOption) = components
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
        debugStartCycle = simConfig.debugStartCycle,
        debugEndCycle = simConfig.debugEndCycle,
        debugMode = simConfig.debugPrint,
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
    }

  }

  private def logSimulation(simConfig: SimulationConfig): Unit = {

    log("[Simulation Overview]]")
    log("\t[Target Layer]")
    log(s"\t\tLayer Name: ${simConfig.layerName}")
    log(s"\t\tM: ${simConfig.layerGemmDimension.m}, N: ${simConfig.layerGemmDimension.n}, K: ${simConfig.layerGemmDimension.k}")
    log(s"\t\tStreaming Dimension Size: ${simConfig.streamingDimensionSize}")
    log("")
    log(s"\t[Off Chip Memory]")
    log(s"\t\tOff Chip Memory Bandwidth: ${simConfig.offChipMemoryBandwidth} bit")
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
    log(s"\t\tSRAM C Single Buffer Limit (KB): ${simConfig.singleBufferLimitKbC} KB")
    log(s"\t\tSRAM C Single Buffer Limit (bit): ${simConfig.singleBufferLimitKbC * 8 * 1024} bit")
    log(s"\t\tSRAM C Total SRAM Size (KB): ${simConfig.singleBufferLimitKbC * 2} KB")
    log(s"\t\tSRAM C Total SRAM Size (bit): ${simConfig.singleBufferLimitKbC * 2 * 8 * 1024} bit")
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

  }

}

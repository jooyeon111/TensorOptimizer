//package simulation
//
//
//import common.{OutputPortCalculator, Dataflow}
//
//import java.io.File
//import scala.annotation.tailrec
//import scala.collection.mutable.ArrayBuffer
//import scala.util.{Failure, Success, Try}
//
//object DesignExplorerMain extends App with Logger with StreamingDimensionCalculator with OutputPortCalculator {
//
//  case class SimulationConfig(
//    layerName: String,
//    layerGemmDimension: GemmDimension,
//    bitWidthPortA: Int,
//    bitWidthPortB: Int,
//    bitWidthPortC: Int,
//    dramBandwidth: Int,
//    totalNumberOfMultipliers: Int,
//    sramEnergies: Vector[SramReferenceData]
//  ) {
//    def validate: Boolean = {
//      layerGemmDimension.validate &&
//      totalNumberOfMultipliers > 0 &&
//      bitWidthPortA > 0 &&
//      bitWidthPortB > 0 &&
//      bitWidthPortC > 0 &&
//      dramBandwidth > 0 &&
//      sramEnergies.forall ( x => x.validate)
//    }
//
//  }
//
//  case class SingleBufferLimitKbs(
//    limitA: Int,
//    limitB: Int,
//    limitC: Int,
//  ) {
//    val limitBitA: Int = limitA * 8 * 1024
//    val limitBitB: Int = limitB * 8 * 1024
//    val limitBitC: Int = limitC * 8 * 1024
//
//    def isAllSramSizeSame: Boolean = (limitA == limitB) && (limitB == limitC)
//
//  }
//
//  case class Architecture(
//    arrayConfig: ArrayConfig,
//    streamingDimensionSize: Int,
//    singleBufferLimitKbs: SingleBufferLimitKbs,
//  ) {
//
//    val dramUploadOrder: DramUploadOrder.Value = arrayConfig.dataflow match {
//      case Dataflow.Is => DramUploadOrder.mkn
//      case Dataflow.Os => DramUploadOrder.mnk
//      case Dataflow.Ws => DramUploadOrder.knm
//    }
//
//    def updateStreamingDimensionSize(nextSize: Int): Architecture = copy(
//      streamingDimensionSize = nextSize
//    )
//
//    def updateSramSize(nextSize: Int): Architecture = copy(
//      singleBufferLimitKbs = SingleBufferLimitKbs(nextSize, nextSize, nextSize)
//    )
//
//    def updatesingleBufferLimitKb(nextSize: Int, sramType: DataType.Value): Architecture = {
//      sramType match {
//        case DataType.A =>
//          copy(
//            singleBufferLimitKbs = SingleBufferLimitKbs(
//            nextSize,
//            this.singleBufferLimitKbs.limitB,
//            this.singleBufferLimitKbs.limitC
//            )
//          )
//
//        case DataType.B =>
//          copy(
//            singleBufferLimitKbs = SingleBufferLimitKbs(
//              this.singleBufferLimitKbs.limitA,
//              nextSize,
//              this.singleBufferLimitKbs.limitC
//            )
//          )
//
//        case DataType.C =>
//          copy(
//            singleBufferLimitKbs = SingleBufferLimitKbs(
//              this.singleBufferLimitKbs.limitA,
//              this.singleBufferLimitKbs.limitB,
//              nextSize
//            )
//          )
//      }
//    }
//
//  }
//
//  case class Result(
//    architecture: Architecture,
//    simulationResult: SimulationResult
//  )
//
//
//  private val help = """
//   |First argument is target MNK layer
//   |Second argument is test setting argument
//   |Third argument is SRAM energy
//  """.stripMargin
//
//  val marginPercent = 50
//  val maximumsingleBufferLimitKb = 1024
//  val minimumsingleBufferLimitKb = 32
//
//  if(args.isEmpty){
//    Console.err.println("No argument is provided" + help)
//    sys.exit(1)
//  } else if (args.length != 3){
//    Console.err.println("Proper arguments are not provided" + help)
//    sys.exit(1)
//  }
//
//  Try {
//
//    val layerConfigParser = new ConfigManager(args(0))
//    val testConfigParser = new ConfigManager(args(1))
//    val sramEnergyParser = new ConfigManager(args(2))
//
//    if(!layerConfigParser.parse()){
//      throw ParseError("Layer parsing fail" + help)
//    }
//
//    if(!testConfigParser.parse()){
//      throw ParseError("Test option parsing fail" + help)
//    }
//
//    if(!sramEnergyParser.parse()){
//      throw ParseError("SRAM Energy configuration parsing failed" + help)
//    }
//
//    val layerConfig = layerConfigParser.getConfig.getOrElse(
//      throw ParseError("Layer config not found")
//    )
//
//    val testConfig = testConfigParser.getConfig.getOrElse(
//      throw ParseError("Test Config not found")
//    )
//
//    val sramConfig = sramEnergyParser.getConfig.getOrElse(
//      throw ParseError("SRAM config not found")
//    )
//
//    val simulationConfig = buildSimulationConfig(layerConfig, testConfig, sramConfig.sramReferenceDataVector)
//    val architectureArrayBuffer = buildInitialArchitecture(simConfig = simulationConfig)
//
//    if(!simulationConfig.validate)
//      throw ParseError("Invalid simulation configuration")
//
//    val loggerOption = setLoggerOption(simulationConfig)
//    logSimulation(simulationConfig)
//
//    log("\n[Simulation START]")
//
//    val optimizedArrayConfigResults = findOptimalArrayConfig(
//      simConfig = simulationConfig,
//      architectureBuffer = architectureArrayBuffer,
//      marginPercent = marginPercent,
//      loggerOption = loggerOption
//    )
//
//    if(optimizedArrayConfigResults.isEmpty){
//      throw RunTimeError("There is no available optimal array configuration candidates...")
//    }
//
//    log("\n[Optimal Config Results]")
//    log(s"Total ${optimizedArrayConfigResults.length} have survived\n")
//
//    optimizedArrayConfigResults.foreach(showResult)
//
//    val optimizedStreamingDimensionResults = findOptimalStreamingSize(
//      simConfig = simulationConfig,
//      resultBuffer = optimizedArrayConfigResults,
//      marginPercent = marginPercent,
//      loggerOption = loggerOption
//    )
//
//    if(optimizedStreamingDimensionResults.isEmpty){
//      throw RunTimeError("There is no available optimal streaming dimension candidates...")
//    }
//
//    log("\n[Optimal Streaming Dimension Results]")
//    log(s"Total ${optimizedStreamingDimensionResults.length} have survived")
//
//    optimizedStreamingDimensionResults.foreach(showResult)
//
//    val optimizedSramArchitectures = findOptimalSramSize(
//      simConfig = simulationConfig,
//      resultBuffer = optimizedStreamingDimensionResults,
//      marginPercent = marginPercent,
//      loggerOption = loggerOption
//    )
//
//    if(optimizedSramArchitectures.isEmpty){
//      throw RunTimeError("There is no available SRAM candidates...")
//    }
//
//    log("\n[Optimal SRAM Size Results]")
//    log(s"Total ${optimizedSramArchitectures.length} have survived")
//
//    optimizedSramArchitectures.foreach(showResult)
//
//    val optimizedSingleSramResults = findOptimalSingleSramSize(
//      simConfig = simulationConfig,
//      resultBuffer = optimizedSramArchitectures,
//      marginPercent = marginPercent,
//      loggerOption = loggerOption
//    )
//
//    if(optimizedSingleSramResults.isEmpty){
//      throw RunTimeError("There is no available single SRAM candidates...")
//    }
//
//    log("\n[Optimal Single SRAM Size Results]")
//    log(s"Total ${optimizedSingleSramResults.length} have survived")
//
//    optimizedSingleSramResults.foreach(showResult)
//
//
//
//  } match {
//
//    case Success(_) =>
//      log("Main End")
//
//    case Failure(e) =>
//      Console.err.println(s"Error: ${e.getMessage}")
//      sys.exit(1)
//
//  }
//
//  private def setLoggerOption(simConfig: SimulationConfig): LoggerOption = {
//
//    val outputMode = OutputMode.File
//    val outputDirectory = "output/"
//    val logFileName = generateLogFileName(simConfig)
//
//    val logFile = new File(outputDirectory + logFileName)
//    val loggerOption = LoggerOption(outputMode, Option(logFile))
//    setMode(loggerOption)
//
//    if(logFile.exists()){
//      if(logFile.delete())
//        println(s"Deleted existing file: $logFileName")
//      else
//        println(s"Failed to delete file: $logFileName")
//    }
//
//    loggerOption
//
//  }
//
//
//  private def buildSimulationConfig(
//    layerConfig: ConfigParser.Config,
//    testConfig: ConfigParser.Config,
//    sramEnergies: Vector[SramReferenceData],
//  ): SimulationConfig ={
//
//    //layer
//    val layerName = layerConfig.getString("Layer Name").getOrElse(
//      throw ParseError("Layer Name not found")
//    )
//    val dimensionM = layerConfig.getInt("M").getOrElse(
//      throw ParseError("M dimension not found")
//    )
//    val dimensionN = layerConfig.getInt("N").getOrElse(
//      throw ParseError("N dimension not found")
//    )
//    val dimensionK = layerConfig.getInt("K").getOrElse(
//      throw ParseError("K dimension not found")
//    )
//    val gemmDimension = GemmDimension(dimensionM, dimensionN, dimensionK)
//
//    val bitWidthPortA = testConfig.getInt("Port A Bit Width").getOrElse(
//      throw ParseError("Port A Bit Width")
//    )
//    val bitWidthPortB = testConfig.getInt("Port B Bit Width").getOrElse(
//      throw ParseError("Port B Bit Width")
//    )
//
//    val bitWidthPortC = testConfig.getInt("Port B Bit Width").getOrElse(
//      throw ParseError("Port B Bit Width")
//    )
//
//    val dramBandwidth = testConfig.getInt("DRAM Bandwidth").getOrElse(
//      throw ParseError("DRAM Bandwidth not found")
//    )
//    val totalNumberOfMultipliers = testConfig.getInt("Total Number of Multipliers").getOrElse(
//      throw ParseError("Total number of multiplier not found")
//    )
//
//    SimulationConfig(
//      layerName = layerName,
//      layerGemmDimension = gemmDimension,
//      bitWidthPortA = bitWidthPortA,
//      bitWidthPortB = bitWidthPortB,
//      bitWidthPortC = bitWidthPortC,
//      dramBandwidth = dramBandwidth,
//      totalNumberOfMultipliers = totalNumberOfMultipliers,
//      sramEnergies = sramEnergies
//    )
//
//  }
//
//
//  def buildInitialArchitecture(simConfig: SimulationConfig): ArrayBuffer[Architecture] = {
//    ArrayBuffer(Dataflow.Is, Dataflow.Os, Dataflow.Ws).flatMap{ dataflow =>
//     ArrayConfigGenerator.generateArrayConfig(
//       multNumber = simConfig.totalNumberOfMultipliers,
//       bandWidthPortA = simConfig.bitWidthPortA,
//       bandWidthPortB = simConfig.bitWidthPortB,
//       dataflow = dataflow,
//       streamingDimensionSize = getMaximumStreamingDimension(simConfig.layerGemmDimension, dataflow)
//     )
//    }.map { arrayConfig =>
//      Architecture(
//        arrayConfig,
//        getMaximumStreamingDimension(simConfig.layerGemmDimension, arrayConfig.dataflow),
//        SingleBufferLimitKbs(maximumsingleBufferLimitKb, maximumsingleBufferLimitKb, maximumsingleBufferLimitKb)
//      )
//    }
//  }
//
//  private def filterArchitectureByCycleThreshold(
//    results: ArrayBuffer[Result],
//    marginPercent: Double = 20.0,
//  ): ArrayBuffer[Result] = {
//
//    val minCycle = results.map(result => result.simulationResult.cycle).min
//    val cycleThreshold = (minCycle * (1 + marginPercent/100)).toLong
//
//    results.filter{ result =>
//      (result.simulationResult.cycle <= cycleThreshold) && (result.simulationResult.cycle != Long.MaxValue)
//    }
//
//  }
//
//  private def findOptimalArrayConfig(
//    simConfig: SimulationConfig,
//    architectureBuffer: ArrayBuffer[Architecture],
//    marginPercent: Double = 20.0,
//    loggerOption: LoggerOption
//  ): ArrayBuffer[Result] = {
//    val result = architectureBuffer.map{ architecture =>
//
//      buildComponents(simConfig = simConfig, architecture = architecture, loggerOption = loggerOption) match {
//        case Right(components) =>
//          Result(architecture, runSimulation(components))
//
//        case Left(_) =>
//          Result(architecture, SimulationResult(wrongCycle = Long.MaxValue))
//      }
//
//    }
//
//    filterArchitectureByCycleThreshold(result, marginPercent)
//  }
//
//
//  private def findOptimalStreamingSize(
//    simConfig: SimulationConfig,
//    resultBuffer: ArrayBuffer[Result],
//    marginPercent: Double = 20.0,
//    loggerOption: LoggerOption,
//  ): ArrayBuffer[Result] = {
//
//    @tailrec
//    def searchStreamingDimensionSizeForArchitecture(result: Result): Result= {
//
//      val currentArchitecture = result.architecture
//      val currentStreamingDimensionSize = result.architecture.streamingDimensionSize
//      val currentSimulationResult = result.simulationResult
//
//      val nextStreamingDimensionSize = currentStreamingDimensionSize / 2
//      val nextArchitecture = currentArchitecture.updateStreamingDimensionSize(nextStreamingDimensionSize)
//
//      val nextSimulationResult = buildComponents(simConfig, nextArchitecture, loggerOption) match {
//        case Right(components) =>
//          runSimulation(components)
//
//        case Left(_) =>
//          SimulationResult(wrongCycle = Long.MaxValue)
//
//      }
//
//      val newResult = Result(nextArchitecture, nextSimulationResult)
//
//      if (nextSimulationResult.cycle <= currentSimulationResult.cycle) {
//        searchStreamingDimensionSizeForArchitecture(newResult)
//      } else {
//        result
//      }
//
//    }
//
//    val result = resultBuffer
//      .map(result =>searchStreamingDimensionSizeForArchitecture(result))
//
//    filterArchitectureByCycleThreshold(result, marginPercent)
//
//  }
//
//  private def findOptimalSramSize(
//    simConfig: SimulationConfig,
//    resultBuffer: ArrayBuffer[Result],
//    marginPercent: Double = 20.0,
//    loggerOption: LoggerOption,
//  ): ArrayBuffer[Result] = {
//
//    @tailrec
//    def searchSramForArchitecture(result: Result): Result = {
//
//      val currentArchitecture = result.architecture
//      val currentSramSize = result.architecture.singleBufferLimitKbs.limitA
//      val currentSimulationResult = result.simulationResult
//
//      assert(currentArchitecture.singleBufferLimitKbs.isAllSramSizeSame,
//        "Search SRAM for archi functions is called in wrong time")
//
//      val nextSramSize = currentSramSize / 2
//
//      if(nextSramSize < minimumsingleBufferLimitKb){
//        result
//      } else {
//
//        val nextArchitecture = currentArchitecture.updateSramSize(nextSramSize)
//        val nextSimulationResult = buildComponents(simConfig, nextArchitecture, loggerOption) match {
//          case Right(components) =>
//            runSimulation(components)
//          case Left(_) =>
//            SimulationResult(wrongCycle = Long.MaxValue)
//        }
//
//        val newResult = Result(nextArchitecture, nextSimulationResult)
//
//        if (nextSimulationResult.cycle <= currentSimulationResult.cycle)
//          searchSramForArchitecture(newResult)
//        else
//          result
//
//      }
//
//    }
//
//    val result = resultBuffer.map(architectureResult => searchSramForArchitecture(architectureResult))
//    filterArchitectureByCycleThreshold(result, marginPercent)
//
//  }
//
//
//  private def findOptimalSingleSramSize(
//    simConfig: SimulationConfig,
//    resultBuffer: ArrayBuffer[Result],
//    marginPercent: Double = 20.0,
//    loggerOption: LoggerOption,
//  ): ArrayBuffer[Result] = {
//
//    @tailrec
//    def searchSingleSramForArchitecture(result: Result): Result = {
//
//      val currentArchitecture = result.architecture
//
//      val currentSramSize = currentArchitecture.arrayConfig.dataflow match {
//        case Dataflow.Is =>
//          result.architecture.singleBufferLimitKbs.limitA
//        case Dataflow.Os =>
//          result.architecture.singleBufferLimitKbs.limitC
//        case Dataflow.Ws =>
//          result.architecture.singleBufferLimitKbs.limitB
//      }
//
//      val currentResult = result.simulationResult
//      val nextSramSize = currentSramSize / 2
//
//      if(nextSramSize < minimumsingleBufferLimitKb){
//        result
//      } else {
//
//        val nextArchitecture = currentArchitecture.arrayConfig.dataflow match {
//          case Dataflow.Is =>
//            currentArchitecture.updatesingleBufferLimitKb(nextSramSize, DataType.A)
//          case Dataflow.Os =>
//            currentArchitecture.updatesingleBufferLimitKb(nextSramSize, DataType.C)
//          case Dataflow.Ws =>
//            currentArchitecture.updatesingleBufferLimitKb(nextSramSize, DataType.B)
//        }
//
//        val newResult = buildComponents(simConfig, nextArchitecture, loggerOption) match {
//          case Right(components) =>
//            runSimulation(components)
//
//          case Left(_) =>
//            SimulationResult(wrongCycle = Long.MaxValue)
//        }
//
//        val newArchitectureResult = Result(nextArchitecture, newResult)
//
//        if (newResult.cycle <= currentResult.cycle)
//          searchSingleSramForArchitecture(newArchitectureResult)
//        else
//          result
//
//      }
//
//    }
//
//    val result = resultBuffer.map(result => searchSingleSramForArchitecture(result))
//    filterArchitectureByCycleThreshold(result, marginPercent)
//
//  }
//
//  private def buildComponents(
//    simConfig: DesignExplorerMain.SimulationConfig,
//    architecture: Architecture,
//    loggerOption: LoggerOption,
//  ):Either[CompBuildError, (Layer, Dram, (DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram), Array, Interface, Vector[SramReferenceData], LoggerOption)] = {
//
//
//    val layer = new Layer(
//      layerName = simConfig.layerName,
//      gemmDimension = simConfig.layerGemmDimension,
//      arrayConfig = architecture.arrayConfig,
//      streamingDimensionSize = architecture.streamingDimensionSize,
//      dramUploadOrder = architecture.dramUploadOrder,
//      loggerOption = loggerOption
//    )
//
//    val sizeTileA = layer.operationVector.head.generateTileA.dims.memorySize
//    val sizeTileB = layer.operationVector.head.generateTileB.dims.memorySize
//    val sizeTileC = layer.operationVector.head.generateTileC.dims.memorySize
//
//    val dram = new Dram(
//      outputBandwidth = simConfig.dramBandwidth, loggerOption = loggerOption
//    )
//
//    val array = new Array(
//      arrayConfig = architecture.arrayConfig,
//      loggerOption = loggerOption
//    )
//
//    buildSrams(
//      arrayConfig = architecture.arrayConfig,
//      simConfig = simConfig,
//      singleBufferLimitKbs = architecture.singleBufferLimitKbs,
//      sizeTileA = sizeTileA,
//      sizeTileB = sizeTileB,
//      sizeTileC = sizeTileC,
//      loggerOption = loggerOption
//    ) match {
//
//      case Right(srams@(sramA, sramB, sramC)) =>
//        val interface = new Interface(
//          dram = dram,
//          sramA = sramA,
//          sramB = sramB,
//          sramC = sramC,
//          array = array
//        )
//        Right((layer, dram, srams, array, interface, simConfig.sramEnergies, loggerOption))
//
//
//      case Left(error) =>
//        println(s"Failed to build hardware components array configuration: ${architecture.arrayConfig.arrayConfigString}")
//        Left(error)
//
//    }
//
//
//  }
//
//  private def buildSrams(
//    arrayConfig: ArrayConfig,
//    simConfig: SimulationConfig,
//    singleBufferLimitKbs: SingleBufferLimitKbs,
//    sizeTileA: Int,
//    sizeTileB: Int,
//    sizeTileC: Int,
//    loggerOption: LoggerOption
//  ): Either[CompBuildError,(DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram)] = {
//
//    val capacityA = singleBufferLimitKbs.limitBitA / sizeTileA
//    val capacityB = singleBufferLimitKbs.limitBitB / sizeTileB
//    val capacityC = singleBufferLimitKbs.limitBitC / sizeTileC
//
//    if(!(capacityA > 0 && capacityB > 0 && capacityC > 0 )) {
//      println(s"Building SRAM is failed: ${arrayConfig.arrayConfigString}")
//      Left(CompBuildError("SRAM Cannot contain even 1 tile increase SRAM size"))
//    } else {
//      val sramA = new DoubleBufferSram(
//        dataType = DataType.A,
//        outputBandwidth = arrayConfig.bandwidthOfInputA,
//        singleBufferTileCapacity = capacityA,
//        singleBufferLimitKb = singleBufferLimitKbs.limitA,
//        loggerOption = loggerOption
//      )
//      val sramB = new DoubleBufferSram(
//        dataType = DataType.B,
//        outputBandwidth = arrayConfig.bandwidthOfInputB,
//        singleBufferTileCapacity = capacityB,
//        singleBufferLimitKb = singleBufferLimitKbs.limitB,
//        loggerOption = loggerOption
//      )
//      val sramC = new OutputDoubleBufferSram(
//        outputBandwidth = simConfig.dramBandwidth,
//        singleBufferTileCapacity = capacityC,
//        singleBufferLimitKb = singleBufferLimitKbs.limitC,
//        loggerOption = loggerOption
//      )
//
//      Right((sramA, sramB, sramC))
//
//    }
//
//
//  }
//
//  private def runSimulation(
//    components: (Layer, Dram, (DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram), Array, Interface, Vector[SramReferenceData] ,LoggerOption)
//  ):  SimulationResult = {
//    val (layer, dram, srams, array, interface, sramData, loggerOption) = components
//    val (sramA, sramB, sramC) = srams
//
//    val compiler = new Compiler(
//      dram = dram,
//      sramA = sramA,
//      sramB = sramB,
//      sramC = sramC,
//      interface = interface,
//      layer = layer,
//      array = array,
//      sramDataReferenceVector = sramData,
//      loggerOption = loggerOption
//    )
//
//
//    try {
//      compiler.run()
//
//      val tileA = layer.operationVector.head.generateTileA
//      val tileB = layer.operationVector.head.generateTileB
//      val tileC = layer.operationVector.head.generateTileC
//
//      SimulationResult(
//        totalOperationNumber = compiler.getTotalOperationNumber,
//        tileSizeA = compiler.getTileSizeA,
//        tileSizeB = compiler.getTileSizeB,
//        tileSizeC = compiler.getTileSizeC,
//        trimTileCountA = compiler.getTrimTileCountA,
//        trimTileCountB = compiler.getTrimTileCountB,
//        singleBufferTileCapacityA = compiler.getSingleBufferTileCapacityA,
//        singleBufferTileCapacityB = compiler.getSingleBufferTileCapacityB,
//        singleBufferTileCapacityC = compiler.getSingleBufferTileCapacityC,
//
//        cycle = compiler.getTotalCycle,
//        arrayActiveCount = compiler.getArrayActiveCount,
//        arrayHoldUpCount = compiler.getArrayHoldUpCount,
//        dramHolUpCount = compiler.getDramHoldUpCount,
//
//        dramLogs = compiler.getDramLogs,
//        dramReadAccessCount = compiler.getDramReadAccessCount,
//        dramWriteAccessCount = compiler.getDramWriteAccessCount,
//        sramReadAccessCountA = compiler.getSramReadAccessCountA,
//        sramWriteAccessCountA = compiler.getSramWriteAccessCountA,
//        sramReadAccessCountB = compiler.getSramReadAccessCountB,
//        sramWriteAccessCountB = compiler.getSramWriteAccessCountB,
//
//        dramHitRatio = compiler.getTotalDramHitCount,
//        dramMissRatio = compiler.getTotalDramMissCount,
//
//        sramHitRatioA = compiler.getSramHitRatioA,
//        sramHitRatioB = compiler.getSramHitRatioB,
//        sramMissRatioA = compiler.getSramMissRatioA,
//        sramMissRatioB = compiler.getSramMissRatioB,
//        sramHitRatio = compiler.getTotalSramHitRatio,
//        sramMissRatio = compiler.getTotalSramMissRatio,
//
//        sramBufferToggleCountA = compiler.getBufferSwapCountA,
//        sramBufferToggleCountB = compiler.getBufferSwapCountB,
//        sramBufferToggleCountC = compiler.getBufferSwapCountC,
//
//        averageMemoryUsageKbA = compiler.getAverageMemoryUsageKbA,
//        averageMemoryUtilizationA = compiler.getAverageMemoryUtilizationA,
//        averageMemoryUsageKbB = compiler.getAverageMemoryUsageKbB,
//        averageMemoryUtilizationB = compiler.getAverageMemoryUtilizationB,
//        averageMemoryUsageKbC = compiler.getAverageMemoryUsageKbC,
//        averageMemoryUtilizationC = compiler.getAverageMemoryUtilizationC,
//
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
//        dramLeakageEnergyPj = compiler.getDramLeakageEnergy,
//        dramEnergyPj = compiler.getDramEnergy,
//
//        arrayDynamicEnergyPj = compiler.getArrayDynamicEnergy,
//        arrayLeakageEnergyPj = compiler.getArrayLeakageEnergy,
//        arrayEnergy = compiler.getArrayEnergy,
//
//        energyPj = compiler.getTotalEnergy,
//      )
//
//    } catch {
//      case _: RunTimeError =>
//        SimulationResult(wrongCycle = Long.MaxValue)
//    }
//
//  }
//
//  private def generateLogFileName(config: SimulationConfig): String = {
//    s"result_${config.layerName}_dram_bandwidth:${config.dramBandwidth}_mult:${config.totalNumberOfMultipliers}"
//  }
//
//  private def logSimulation(simConfig: SimulationConfig): Unit = {
//
//    log("[Cycle Simulation Overview]")
//    log("")
//    log("[Target Layer]")
//    log(s"\tLayer Name: ${simConfig.layerName}")
//    log(s"\tM: ${simConfig.layerGemmDimension.m}, " +
//      s"N: ${simConfig.layerGemmDimension.n}, " +
//      s"K: ${simConfig.layerGemmDimension.k} ")
//    log("")
//    log(s"[DRAM]")
//    log(s"\tDRAM Bandwidth: ${simConfig.dramBandwidth}")
//    log("")
//    log(s"[Systolic Tensor Array]")
//    log(s"\tPort A Bit Width: ${simConfig.bitWidthPortA}")
//    log(s"\tPort B Bit Width: ${simConfig.bitWidthPortB}")
//    log(s"\tTotal Number of Multipliers: ${simConfig.totalNumberOfMultipliers}")
//  }
//
//}

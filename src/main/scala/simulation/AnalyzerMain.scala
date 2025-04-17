package simulation

import common.FilePaths
import scala.util.{Failure, Success, Try}
import java.io.File

object AnalyzerMain extends App
  with RtlSynthesisManager
  with SingleLayerSimulation
  with Logger {

  private val help = """
    |Usage:
    |[1 argument] - RTL Synthesis Manager Mode:
    |  First argument is for calculating proper band width
    |
    |[2 arguments] - Cycle Report Only Mode:
    |  First argument is target MNK layer
    |  Second argument is test setting argument
    |
    |[3 arguments] - ML Model Training Mode:
    |  First argument is training CSV path
    |  Second argument is validation CSV path
    |  Third argument is test CSV path (output weight file will be 'weight.bin')
    |
    |[5 arguments] - Cycle and Energy Report Mode (with array data or ML inference):
    |  First argument is target MNK layer
    |  Second argument is test setting argument
    |  Third argument is off chip memory Reference Data
    |  Fourth argument is SRAM Reference Data
    |  Fifth argument is either Array Synthesis Reference Data or ML weight file (.bin)
  """.stripMargin

  if(args.isEmpty){
    Console.err.println("No argument is provided" + help)
    sys.exit(1)
  }

  println("=" * 30 + "Analyzer Main START" + "=" * 30)

  Try {

    if(args.length == 1){
      println("RTL Synthesis Manger Mode")
      processArrayConfigsAndGenerateRtl(args(0), help)
    } else if (args.length == 2) {
      println("Cycle Report Only")
      runLayerSimulation(
        layerPath = args(0),
        testPath = args(1),
        help = help
      )
    } else if(args.length == 3){

      println("ML Model Training Mode")
      val trainCsvPath = args(0)
      val validationCsvPath = args(1)
      val testCsvPath = args(2)
      val outputWeightPath = FilePaths.resourcesInputSimulation + "/synthesis/weight.bin"

      println(s"Training ML model using:")
      println(s"- Training data: $trainCsvPath")
      println(s"- Validation data: $validationCsvPath")
      println(s"- Test data: $testCsvPath")
      println(s"- Output weight file: $outputWeightPath")

      val loggerOption = LoggerOption(OutputMode.Console, None)

      DNNPredictor.trainModel(
        weightOutputPath = outputWeightPath,
        trainFilePath = trainCsvPath,
        validationFilePath = validationCsvPath,
        testFilePath = testCsvPath,
        loggerOption = loggerOption
      ) match {
        case Success(_) =>
          println(s"Successfully trained and saved ML model to $outputWeightPath")
        case Failure(e) =>
          Console.err.println(s"Failed to train ML model: ${e.getMessage}")
          sys.exit(1)
      }

    } else if (args.length == 5){

//      val file = new File(args(4))
//      if(file.exists()){
//        println("weight file is real")
//      } else {
//        println("weight file is not real")
//      }

      if (args(4).endsWith(".bin")) {
        println("Cycle and Energy Report Mode with ML Inference")
        // Load existing ML model weights
        val loggerOption = LoggerOption(OutputMode.Console, None)

        DNNPredictor.loadModel(
          filePath = args(4),
          loggerOption = loggerOption
        ) match {
          case Success(modelWeights) =>

            runLayerSimulation(
              layerPath = args(0),
              testPath = args(1),
              offChipMemoryDataPath = Option(args(2)),
              sramDataPath = Option(args(3)),
              dnnModelWeights = Some(modelWeights),
              help = help
            )

          case Failure(e) =>
            Console.err.println(s"Failed to load ML model: ${e.getMessage}")
            sys.exit(1)

        }

      } else {
        println("Cycle and Energy Report Mode with Design Compiler Results")
        runLayerSimulation(
          layerPath = args(0),
          testPath = args(1),
          offChipMemoryDataPath = Option(args(2)),
          sramDataPath = Option(args(3)),
          arrayDataPath = Option(args(4)),
          help = help
        )
      }

    } else {
      Console.err.println(s"Invalid number of arguments It is ${args.length}" + help)
      sys.exit(1)
    }

  } match {
    case Success(_) =>
      println("=" * 30 + "Analyzer Main END" + "="*30)
    case Failure(e) =>
      Console.err.println(s"Error: ${e.getMessage}")
      sys.exit(1)
  }

}
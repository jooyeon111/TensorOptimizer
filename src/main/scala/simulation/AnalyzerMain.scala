package simulation

import scala.util.{Failure, Success, Try}

object AnalyzerMain extends App
  with RtlGenerationManager
  with SingleLayerSimulation
  with Logger {


  //TODO immigrate RTL synthesis mode to MXU Forge
  private val help = """
    |Usage:
    |[1 argument] - RTL Synthesis Manager Mode:
    |  First argument is for calculating proper band width
    |
    |[2 arguments] - Cycle Report Only Mode:
    |  First argument is target MNK layer
    |  Second argument is test setting argument
    |  Third argument is tiling argument
    |
    |[4 arguments] - Cycle and Energy Report Mode (with Few shot model):
    |  First argument is target MNK layer
    |  Second argument is test setting argument
    |  Third argument is tiling argument
    |  Fourth argument is off chip memory Reference Data
    |  Fifth argument is SRAM Reference Data
    |
    |[5 arguments] - Cycle and Energy Report Mode (with array data):
    |  First argument is target MNK layer
    |  Second argument is test setting argument
    |  Third argument is tiling argument
    |  Fourth argument is off chip memory Reference Data
    |  Fifth argument is SRAM Reference Data
    |  Sixth argument is either Array Synthesis Reference Data
    |
  """.stripMargin

  if(args.isEmpty){
    Console.err.println("No argument is provided" + help)
    sys.exit(1)
  }

  println("=" * 30 + "Analyzer Main START" + "=" * 30)

  Try {

    if(args.length == 1){
      println("RTL Synthesis Manger Mode")
      generateRtl(args(0), help)
    } else if (args.length == 3) {
      println("Cycle Report Only")
      runLayerSimulation(
        layerPath = args(0),
        testPath = args(1),
        tilingPath = args(2),
        help = help
      )
    } else if(args.length == 5){

      println("Cycle and Energy Report Mode with deep learning")
      FewShotPredictor.loadModelFromDefaultFiles match {
        case Success(_) =>
          runLayerSimulation(
            layerPath = args(0),
            testPath = args(1),
            tilingPath = args(2),
            offChipMemoryDataPath = Option(args(3)),
            sramDataPath = Option(args(4)),
            help = help
          )

        case Failure(e) =>
          Console.err.println(s"Failed to load ML model: ${e.getMessage}")
          sys.exit(1)
      }

    } else if (args.length == 6){

      println("Cycle and Energy Report Mode with Design Compiler Results")
      runLayerSimulation(
        layerPath = args(0),
        testPath = args(1),
        tilingPath = args(2),
        offChipMemoryDataPath = Option(args(3)),
        sramDataPath = Option(args(4)),
        arrayDataPath = Option(args(5)),
        help = help
      )


    } else {
      Console.err.println(s"Invalid number of arguments It is ${args.length}" + help)
      args.foreach(Console.err.println)
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
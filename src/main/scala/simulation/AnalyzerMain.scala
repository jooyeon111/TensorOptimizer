package simulation

import scala.util.{Failure, Success, Try}

object AnalyzerMain extends App
  with RtlSynthesisManager
  with SingleLayerSimulation
  with Logger {

  private val help = """
    |Usage 1: [three config files mode]
    |First argument is target MNK layer
    |Second argument is test setting argument
    |Third argument is DRAM Reference Data (Option)
    |Fourth argument is SRAM Reference Data (Option)
    |Fifth argument is Array Synthesis Reference Data (Option)
    |Usage 2: [single config file mode]
    |First argument is for calculating proper band width
  """.stripMargin

  if(args.isEmpty){
    Console.err.println("No argument is provided" + help)
    sys.exit(1)
  }

  println("=" * 30 + "Analyzer Main START" + "=" * 30)

  Try {

    if(args.length == 1){
      processArrayConfigsAndGenerateRtl(args(0), help)
    } else if (args.length == 2) {
      println("Cycle Report Only")
      processOneLayer(
        layerPath = args(0),
        testPath = args(1),
        help = help
      )
    } else if (args.length == 4){
      println("Cycle and Energy Report")
      println("Array Energy and Area will be calculated automatically")
      processOneLayer(
        layerPath = args(0),
        testPath = args(1),
        dramDataPath = Option(args(2)),
        sramDataPath = Option(args(3)),
        help = help
      )
    } else if (args.length ==5){
      println("Cycle and Energy Report")
      processOneLayer(
        layerPath = args(0),
        testPath = args(1),
        dramDataPath = Option(args(2)),
        sramDataPath = Option(args(3)),
        arrayDataPath = Option(args(4)),
        help = help
      )
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
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
    |Third argument is SRAM Reference Data
    |Fourth argument is DRAM Reference Data
    |Fifth argument is Array Reference Data
    |
    |Usage 2: [single config file mode]
    |First argument is for calculating proper band width
  """.stripMargin

  if(args.isEmpty){
    Console.err.println("No argument is provided" + help)
    sys.exit(1)
  }

  println("Debug Main START")

  Try {

    if(args.length == 1){
      processArrayConfigsAndGenerateRtl(args(0), help)
    } else if (args.length == 5)
      processOneLayer(args(0), args(1), args(2), args(3), args(4), help)
    else {
      Console.err.println(s"Invalid number of arguments It is ${args.length}" + help)
      sys.exit(1)
    }

  } match {
    case Success(_) =>
      println("Analyzer Main End")
    case Failure(e) =>
      Console.err.println(s"Error: ${e.getMessage}")
      sys.exit(1)
  }



}
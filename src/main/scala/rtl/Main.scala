package rtl

import scala.util.{Failure, Success}
import common.RtlGenerator

object Main extends App with RtlGenerator with ConfigurationParser {
  parseArgs(args) match {
    case Right(fileName) =>
      ConfigParser.parseConfigFile(fileName) match {
        case Success(config) =>
          parseConfig(config) match {
            case Success(verilogGenerationConfig) => generateRtl(verilogGenerationConfig)
            case Failure(e) => Console.err.println(s"Error parsing config: ${e.getMessage}")
          }
        case Failure(e) => Console.err.println(s"Cannot read config parser file ${e.getMessage}")
      }
    case Left(error) => Console.err.println(error + "\nPut Systolic tensor array configuration files in resource directory")
  }
}

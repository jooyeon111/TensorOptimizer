package simulation

import scala.util.{Failure, Success}

class ConfigManager(val fileName: String) {
  private var config: Option[ConfigParser.Config] = None

  def parse(): Boolean = {
    ConfigParser.Config.parseConfigFile(fileName) match {
      case Success(conf) =>
        config = Some(conf)
        true
      case Failure(exception) =>
        Console.err.println(s"Failed to load configuration for layer parser: ${exception.getMessage}")
        false
    }
  }

  def getConfig: Option[ConfigParser.Config] = config

}

object ConfigManager {
  def apply(fileName: String): ConfigManager = new ConfigManager(fileName)
}
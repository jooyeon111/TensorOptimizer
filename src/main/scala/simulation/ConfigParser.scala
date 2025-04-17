package simulation

import scala.io.Source
import scala.util.Try

object ConfigParser {
  case class Config(
     values: Map[String, String],
     sramReferenceDataVector: Vector[SramReferenceData] = Vector.empty,
  ) {
    def getString(key: String): Option[String] = values.get(key)
    def getInt(key: String): Option[Int] = values.get(key).flatMap(v => Try(v.toInt).toOption)
    def getDouble(key: String): Option[Double] = values.get(key).flatMap(v => Try(v.toDouble).toOption)
    def getBoolean(key: String): Option[Boolean] = values.get(key).flatMap( v =>Try(v.toBoolean).toOption)
  }

  object Config {
    def parseConfigFile(fileName: String): Try[Config] = {
      Try {
        val cfgSource = Source.fromResource(fileName)
        val lines = cfgSource.getLines().filterNot(_.trim.isEmpty)

        // Initialize accumulators
        var configMap = Map[String, String]()
        var sramDataVector = Vector[SramReferenceData]()
        var parsingSection = ""

        // Process each line
        lines.foreach { line =>
          line.trim match {
            case "[SRAM REPORT]" => parsingSection = "SRAM"
            case l if l.startsWith("#") => // Skip comments
            case l if !l.startsWith("[") =>
              parsingSection match {
                case "SRAM" =>
                  // Parse SRAM energy data
                  val parts = l.trim.split("\\s+")
                  if (parts.length == 6) {
                    Try {
                      val sramData = SramReferenceData(
                        capacityKb = parts(0).toInt,
                        bandwidthBytes = parts(1).toInt,
                        readEnergyPj = parts(2).toDouble,
                        writeEnergyPj = parts(3).toDouble,
                        leakagePowerMw = parts(4).toDouble,
                        areaUm2 = parts(5).toDouble
                      )
                      sramDataVector = sramDataVector :+ sramData
                    }.recover {
                      case _: Exception =>
                        println(s"Warning: Failed to parse SRAM energy line: $l")
                    }
                  }

                case _ if l.contains(":") =>
                  // Parse regular config key-value pair
                  val Array(key, value) = l.split(":", 2).map(_.trim)
                  configMap += (key -> value)

                case _ => // Skip unrecognized lines
              }
            case _ => // Skip other lines
          }
        }

        cfgSource.close()
        Config(configMap, sramDataVector)

      }
    }
  }
}
package simulation

import java.io.File

trait ConfigurationWriter { self: Logger with BandWidthFilter =>

  def writeConfigurationFile(arrayConfigs: Vector[ArrayConfig], outputPath: String): Unit = {
    // Filter configurations with two sigma
    val filteredConfigs = filterConfigsWithTwoSigma(arrayConfigs)

    // Generate configuration content for each filtered config
    filteredConfigs.foreach { config =>

      val arrayConfigString = s"${config.groupPeRow}x${config.groupPeCol}x${config.vectorPeRow}x${config.vectorPeCol}x${config.numMultiplier}"

      val fileName = config.dataflow match {
        case Dataflow.Is =>
          s"${outputPath}/is_$arrayConfigString.cfg"
        case Dataflow.Os =>
          s"${outputPath}/os_$arrayConfigString.cfg"
        case Dataflow.Ws =>
          s"${outputPath}/ws_$arrayConfigString.cfg"
      }
      val content = generateConfigContent(config)

      writeToFile(fileName, content)
    }
  }

  private def generateConfigContent(config: ArrayConfig): String = {
    val sb = new StringBuilder

    // Header
    sb.append("[Systolic Tensor Array Configuration]\n")
    sb.append("\n")

    // Dataflow type comments
    sb.append("#IS: Input Stationary\n")
    sb.append("#OS: Output Stationary\n")
    sb.append("#WS: Weight Stationary\n")
    sb.append("\n")

    // Current dataflow configuration
    sb.append(s"Dataflow: ${config.dataflow match {
      case Dataflow.Is => "IS"
      case Dataflow.Os => "OS"
      case Dataflow.Ws => "WS"
    }}\n")
    sb.append("\n")

    // Array configuration
    sb.append(s"R: ${config.groupPeRow}\n")
    sb.append(s"C: ${config.groupPeCol}\n")
    sb.append(s"A: ${config.vectorPeRow}\n")
    sb.append(s"B: ${config.vectorPeCol}\n")
    sb.append(s"P: ${config.numMultiplier}\n")
    sb.append("\n")

    // Port Configuration section
    sb.append("[Port Configuration]\n")
    sb.append("Port Type: Signed\n")
    sb.append("#Port Type: Unsigned\n")
    sb.append(s"Port A: ${config.portBitWidth.typeA}\n")
    sb.append(s"Port B: ${config.portBitWidth.typeB}\n")
    sb.append(s"#Port C: ${config.portBitWidth.typeC}\n")

    // Verilog Generation Option
    sb.append("\n")
    sb.append("[Verilog Generation Option]\n")
    sb.append("Split Verilog Output: false\n")

    sb.toString
  }

  private def writeToFile(filePath: String, content: String): Unit = {
    try {
      val file = new File(filePath)
      // Create parent directories if they don't exist
      file.getParentFile.mkdirs()

      val writer = new java.io.PrintWriter(file)
      try {
        writer.write(content)
      } finally {
        writer.close()
      }
      log(s"Successfully wrote configuration to: $filePath")
    } catch {
      case e: Exception =>
        log(s"Error writing configuration file: ${e.getMessage}")
        throw e
    }
  }
}
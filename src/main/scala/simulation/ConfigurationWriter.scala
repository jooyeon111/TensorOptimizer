package simulation

import java.io.File
import common.{Dataflow, FilePaths}

trait ConfigurationWriter { self: Logger with BandWidthFilter =>

  def writeConfigurationFile(arrayConfigs: Vector[ArrayConfig]): Unit = {
    // Filter configurations with two sigma
    val filteredConfigs = filterConfigsWithTwoSigma(arrayConfigs)
//    val outputPath = "src/main/resources/rtl"
    // Generate configuration content for each filtered config
    filteredConfigs.foreach { config =>

      val arrayConfigString = s"${config.groupPeRow}x${config.groupPeCol}x${config.vectorPeRow}x${config.vectorPeCol}x${config.numMultiplier}"

      val fileName = config.dataflow match {
        case Dataflow.Is =>
          s"${FilePaths.resourcesInputRtl}/is_$arrayConfigString.cfg"
        case Dataflow.Os =>
          s"${FilePaths.resourcesInputRtl}/os_$arrayConfigString.cfg"
        case Dataflow.Ws =>
          s"${FilePaths.resourcesInputRtl}/ws_$arrayConfigString.cfg"
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

  def generateMakefile(arrayConfigs: Vector[ArrayConfig]): Unit = {
    val sb = new StringBuilder

    // Add header and default target
    sb.append("# Makefile for RTL Generation\n\n")
    sb.append(".PHONY: all clean rtl_gen\n\n")
    sb.append("all: rtl_gen\n\n")

    // Generate individual RTL generation targets
    arrayConfigs.foreach { config =>
      val arrayConfigString = s"${config.groupPeRow}x${config.groupPeCol}x${config.vectorPeRow}x${config.vectorPeCol}x${config.numMultiplier}"
      val configName = config.dataflow match {
        case Dataflow.Is => s"is_$arrayConfigString"
        case Dataflow.Os => s"os_$arrayConfigString"
        case Dataflow.Ws => s"ws_$arrayConfigString"
      }

      sb.append(s"rtl_gen_$configName:\n")
      //TODO how to unify directory
      sb.append(s"\tsbt \"runMain rtl.Main input/rtl/$configName.cfg\"\n\n")
    }

    // Add main rtl_gen target that depends on all individual targets
    sb.append("rtl_gen: ")
    sb.append(arrayConfigs.map { config =>
      val arrayConfigString = s"${config.groupPeRow}x${config.groupPeCol}x${config.vectorPeRow}x${config.vectorPeCol}x${config.numMultiplier}"
      val configName = config.dataflow match {
        case Dataflow.Is => s"is_$arrayConfigString"
        case Dataflow.Os => s"os_$arrayConfigString"
        case Dataflow.Ws => s"ws_$arrayConfigString"
      }
      s"rtl_gen_$configName"
    }.mkString(" "))
    sb.append("\n\n")

    // Add clean target
    sb.append("clean:\n")
    sb.append("\trm -rf rtl/*.v\n")

    // Write Makefile
    writeToFile("Makefile.automatic_rtl", sb.toString)
  }


  private def writeToFile(filePath: String, content: String): Unit = {
    try {
      val file = new File(filePath)

      val parentFile = file.getParentFile
      if(parentFile != null){
        parentFile.mkdirs()
      }

//      file.getParentFile.mkdirs()

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
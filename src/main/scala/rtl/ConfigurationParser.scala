package rtl

import chisel3.util.log2Ceil
import common.{ArrayDimension, Dataflow, IntegerType, OutputPortCalculator, PortBitWidthInfo, VerilogGenerationConfig}

import scala.util.Try

trait ConfigurationParser extends OutputPortCalculator {

  def parseArgs(args: Array[String]): Either[String, String] = {
    args match {
      case Array(fileName) => Right(fileName)
      case Array() => Left("No argument is provided")
      case _ => Left("Too many arguments are provided")
    }
  }

  def parseConfig(config: ConfigParser.Config): Try[VerilogGenerationConfig] = Try {

    val splitVerilogOutput = config.getBoolean("Split Verilog Output").get

    val dataflow = config.getString("Dataflow").get match {
      case "IS" => Dataflow.Is
      case "OS" => Dataflow.Os
      case "WS" => Dataflow.Ws
      case _ => throw new IllegalArgumentException("Invalid dataflow")
    }

    val arrayDimension = new ArrayDimension(
      config.getInt("R").get,
      config.getInt("C").get,
      config.getInt("A").get,
      config.getInt("B").get,
      config.getInt("P").get,
    )

    val integerType = config.getString("Port Type").get match {
      case "Unsigned" => IntegerType.UnSigned
      case "Signed" => IntegerType.Signed
      case _ =>
        Console.err.println("Invalid integer type")
        sys.exit(1)
    }

    val bitWidthPortA = config.getInt("Port A").get
    val bitWidthPortB = config.getInt("Port B").get
    val bitWidthPortC = config.getInt("Port C")

    val portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = dataflow,
      groupPeRow = arrayDimension.groupPeRow,
      groupPeCol = arrayDimension.groupPeCol,
      vectorPeRow = arrayDimension.vectorPeRow,
      vectorPeCol = arrayDimension.vectorPeCol,
      numMultiplier = arrayDimension.numMultiplier,
      bitWidthPortA = bitWidthPortA,
      bitWidthPortB = bitWidthPortB,
      configPortC = bitWidthPortC,
    )

    VerilogGenerationConfig(
      splitVerilogOutput = splitVerilogOutput,
      dataflow = dataflow,
      arrayDimension = arrayDimension,
      integerType = integerType,
      portBitWidthInfo = portBitWidthInfo
    )

  }

}

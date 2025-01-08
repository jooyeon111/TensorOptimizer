package simulation

import java.io.{File, FileWriter, PrintWriter}

trait Logger {

  protected var mode: OutputMode.Value = OutputMode.Console
  protected var outputFile: Option[File] = None

  protected def setMode(loggerOption: LoggerOption): Unit = {
    mode = loggerOption.outputMode
    outputFile = loggerOption.file
  }

  protected def log(message: String): Unit = {
    mode match {
      case OutputMode.Console =>
        println(message)
      case OutputMode.File =>
        outputFile match {
          case Some(file) =>
            val writer = new PrintWriter(new FileWriter(file, true))
            try {
              writer.println(message)
            } finally {
              writer.close()
            }
          case None =>
            println(message)
            Console.err.println(s"[error] Output file must be set when mode is file")
            sys.exit(1)
        }
    }
  }

  protected def logWithoutNewLine(message: String): Unit = {
    mode match {
      case OutputMode.Console =>
        println(message)
      case OutputMode.File =>
        outputFile match {
          case Some(file) =>
            val writer = new PrintWriter(new FileWriter(file, true))
            try {
              writer.print(message)
            } finally {
              writer.close()
            }
          case None =>
            println(message)
            Console.err.println(s"[error] Output file must be set when mode is file")
            sys.exit(1)
        }
    }
  }

}


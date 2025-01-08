package simulation

import java.io.File

case class LoggerOption(outputMode: OutputMode.Value, file: Option[File])

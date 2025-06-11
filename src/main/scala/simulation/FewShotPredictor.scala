package simulation

import scala.util.{Try, Success, Failure}
import scala.io.Source
import scala.Array

object FewShotPredictor {

  // Model case class to hold the loaded coefficients and metadata
  case class Model(
                    areaCoefficients: Array[Array[Double]],
                    powerCoefficients: Array[Array[Double]],
                    areaFeatureStats: Map[String, FeatureStats],
                    powerFeatureStats: Map[String, FeatureStats],
                    dataflowMapping: Map[String, Int],
                    featureColumns: Array[String]
                  )

  case class FeatureStats(min: Double, max: Double, range: Double)

  case class InputFeatures(
                            dataflow: String,
                            totalNumberOfMultipliers: Int,
                            r: Int,
                            c: Int,
                            a: Int,
                            b: Int,
                            p: Int,
                            streamingDimensionSize: Int
                          )

  // Mutable state to hold the loaded model
  private var loadedModel: Option[Model] = None

  // Simple JSON parser for our specific use case
  object SimpleJsonParser {

    def parseArray(jsonStr: String): Array[Array[Double]] = {
      // Remove whitespace and find the coefficients array
      val cleaned = jsonStr.replaceAll("\\s+", "")
      val coeffPattern = "\"coefficients\":\\[(.+?)\\](?:,|})".r

      coeffPattern.findFirstMatchIn(cleaned) match {
        case Some(m) =>
          val arrayContent = m.group(1)
          parseNestedArray(arrayContent)
        case None =>
          throw new RuntimeException("Could not find coefficients array in JSON")
      }
    }

    private def parseNestedArray(content: String): Array[Array[Double]] = {
      // Split by ],[ to get individual arrays
      val arrays = content.split("\\],\\[")
      arrays.map { arrayStr =>
        val cleaned = arrayStr.replaceAll("[\\[\\]]", "")
        cleaned.split(",").map(_.trim.toDouble)
      }
    }

    def parseFeatureColumns(jsonStr: String): Array[String] = {
      val cleaned = jsonStr.replaceAll("\\s+", "")
      val pattern = "\"feature_columns\":\\[(.+?)\\]".r

      pattern.findFirstMatchIn(cleaned) match {
        case Some(m) =>
          val arrayContent = m.group(1)
          arrayContent.split(",").map(_.replaceAll("\"", "").trim)
        case None =>
          throw new RuntimeException("Could not find feature_columns in JSON")
      }
    }

    def parseFeatureStats(jsonStr: String): Map[String, FeatureStats] = {
      val lines = jsonStr.split("\n").map(_.trim).filter(_.nonEmpty)
      var result = Map[String, FeatureStats]()
      var currentKey: String = null
      var min: Double = 0.0
      var max: Double = 0.0
      var range: Double = 0.0

      for (line <- lines) {
        if (line.contains("\"") && line.contains(":") && line.contains("{")) {
          // Extract key name
          val keyPattern = "\"([^\"]+)\":\\s*\\{".r
          keyPattern.findFirstMatchIn(line) match {
            case Some(m) => currentKey = m.group(1)
            case None => // continue
          }
        } else if (line.contains("\"min\":")) {
          val numPattern = "\"min\":\\s*([0-9.-]+)".r
          numPattern.findFirstMatchIn(line) match {
            case Some(m) => min = m.group(1).toDouble
            case None => // continue
          }
        } else if (line.contains("\"max\":")) {
          val numPattern = "\"max\":\\s*([0-9.-]+)".r
          numPattern.findFirstMatchIn(line) match {
            case Some(m) => max = m.group(1).toDouble
            case None => // continue
          }
        } else if (line.contains("\"range\":")) {
          val numPattern = "\"range\":\\s*([0-9.-]+)".r
          numPattern.findFirstMatchIn(line) match {
            case Some(m) =>
              range = m.group(1).toDouble
              if (currentKey != null) {
                result = result + (currentKey -> FeatureStats(min, max, range))
              }
            case None => // continue
          }
        }
      }
      result
    }
  }

  private def loadJsonFile(filePath: String): String = {
    val source = Source.fromResource("input/simulation/synthesis/" + filePath)
    try {
      source.getLines().mkString("\n")
    } finally {
      source.close()
    }
  }

  private def loadModel(
    areaCoefficientsPath: String,
    powerCoefficientsPath: String,
    areaStatsPath: String,
    powerStatsPath: String
  ): Try[Model] = Try {

    // Load area coefficients
    val areaJson = loadJsonFile(areaCoefficientsPath)
    val areaCoefficients = SimpleJsonParser.parseArray(areaJson)
    val featureColumns = SimpleJsonParser.parseFeatureColumns(areaJson)

    // Load power coefficients
    val powerJson = loadJsonFile(powerCoefficientsPath)
    val powerCoefficients = SimpleJsonParser.parseArray(powerJson)

    // Load area feature stats
    val areaStatsJson = loadJsonFile(areaStatsPath)
    val areaFeatureStats = SimpleJsonParser.parseFeatureStats(areaStatsJson)

    // Load power feature stats
    val powerStatsJson = loadJsonFile(powerStatsPath)
    val powerFeatureStats = SimpleJsonParser.parseFeatureStats(powerStatsJson)

    // Create dataflow mapping (inferred from the Python code)
    val dataflowMapping = Map(
      "IS" -> 0,
      "WS" -> 1,
      "OS" -> 2
    )

    val model = Model(
      areaCoefficients = areaCoefficients,
      powerCoefficients = powerCoefficients,
      areaFeatureStats = areaFeatureStats,
      powerFeatureStats = powerFeatureStats,
      dataflowMapping = dataflowMapping,
      featureColumns = featureColumns
    )

    loadedModel = Some(model)
    model
  }

  // Convenience method that uses the JSON files from your upload
  def loadModelFromDefaultFiles: Try[Model] = {
    loadModel(
      "hardware_predictor_area_coefficients.json",
      "hardware_predictor_total_power_coefficients.json",
      "hardware_predictor_area_feature_stats.json",
      "hardware_predictor_total_power_feature_stats.json"
    )
  }

  def predict(features: InputFeatures): Try[ArraySynthesisData] = Try {
    val model = loadedModel.getOrElse(
      throw new IllegalStateException("Model not loaded. Call loadModel() first.")
    )

    // Encode dataflow
    val dataflowEncoded = model.dataflowMapping.getOrElse(features.dataflow, 0)

    // Create feature vector in the same order as training
    val rawFeatures = Array(
      dataflowEncoded.toDouble,
      features.totalNumberOfMultipliers.toDouble,
      features.r.toDouble,
      features.c.toDouble,
      features.a.toDouble,
      features.b.toDouble,
      features.p.toDouble,
      features.streamingDimensionSize.toDouble
    )

    // Normalize features for area prediction
    val normalizedAreaFeatures = normalizeFeatures(rawFeatures, model.areaFeatureStats, model.featureColumns)

    // Normalize features for power prediction
    val normalizedPowerFeatures = normalizeFeatures(rawFeatures, model.powerFeatureStats, model.featureColumns)

    // Make area prediction using ensemble
    val areaPrediction = ensemblePredict(normalizedAreaFeatures, model.areaCoefficients)

    // Make power prediction using ensemble
    val powerPrediction = ensemblePredict(normalizedPowerFeatures, model.powerCoefficients)

    // Convert power from watts to milliwatts and split into components
    // Based on typical hardware patterns, approximate the power breakdown
    val totalPowerMw = powerPrediction / 1000.0 // Convert watts to milliwatts

    ArraySynthesisData(
      areaUm2 = areaPrediction,
      totalPowerMw = totalPowerMw
    )
  }

  // Overloaded predict method that matches your original signature
  def predict(): ArraySynthesisData = {
    // Default test case - you can modify this or make it configurable
    val defaultFeatures = InputFeatures("OS", 256, 16, 16, 1, 1, 1, 1)
    predict(defaultFeatures) match {
      case Success(result) => result
      case Failure(exception) =>
        throw new RuntimeException(s"Prediction failed: ${exception.getMessage}")
    }
  }

  private def normalizeFeatures(rawFeatures: Array[Double],
                                featureStats: Map[String, FeatureStats],
                                featureColumns: Array[String]): Array[Double] = {
    rawFeatures.zip(featureColumns).map { case (value, column) =>
      featureStats.get(column) match {
        case Some(stats) if stats.range > 0 =>
          (value - stats.min) / stats.range
        case _ => 0.0
      }
    }
  }

  private def ensemblePredict(normalizedFeatures: Array[Double],
                              coefficientsMatrix: Array[Array[Double]]): Double = {
    // Add bias term (1.0) to the beginning of features
    val featuresWithBias = 1.0 +: normalizedFeatures

    // Make prediction with each model in the ensemble
    val predictions = coefficientsMatrix.map { coefficients =>
      featuresWithBias.zip(coefficients).map { case (feature, coeff) =>
        feature * coeff
      }.sum
    }

    // Return ensemble average
    predictions.sum / predictions.length
  }

  // Convenience method for quick testing
  def predictFromMap(features: Map[String, Any]): Try[ArraySynthesisData] = Try {
    val inputFeatures = InputFeatures(
      dataflow = features("dataflow").toString,
      totalNumberOfMultipliers = features("totalNumberOfMultipliers").toString.toInt,
      r = features("r").toString.toInt,
      c = features("c").toString.toInt,
      a = features("a").toString.toInt,
      b = features("b").toString.toInt,
      p = features("p").toString.toInt,
      streamingDimensionSize = features("streamingDimensionSize").toString.toInt
    )
    predict(inputFeatures).get
  }

  // Utility method to check if model is loaded
  def isModelLoaded: Boolean = loadedModel.isDefined

  // Utility method to get model info
  def getModelInfo: Option[String] = loadedModel.map { model =>
    s"Area ensemble: ${model.areaCoefficients.length} models, " +
      s"Power ensemble: ${model.powerCoefficients.length} models, " +
      s"Features: ${model.featureColumns.mkString(", ")}"
  }
}

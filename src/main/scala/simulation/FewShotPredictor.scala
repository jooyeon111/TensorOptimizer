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

    // Create dataflow mapping (matching Python exactly)
    val dataflowMapping = Map(
      "IS" -> 0,
      "OS" -> 1,  // Changed from 2 to 1 to match Python
      "WS" -> 2   // Changed from 1 to 2 to match Python
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

  // Feature engineering function that matches Python exactly
  private def engineerFeatures(features: InputFeatures): Array[Double] = {
    val dataflowEncoded = loadedModel.get.dataflowMapping.getOrElse(features.dataflow, 0).toDouble

    // Raw features
    val totalMult = features.totalNumberOfMultipliers.toDouble
    val r = features.r.toDouble
    val c = features.c.toDouble
    val a = features.a.toDouble
    val b = features.b.toDouble
    val p = features.p.toDouble
    val streamingSize = features.streamingDimensionSize.toDouble

    // Engineered features (matching Python exactly)
    val rCInteraction = r * c
    val aBPInteraction = a * b * p
    val rAPInteraction = r * a * p
    val cBPInteraction = c * b * p
    val rCABInteraction = r * c * a * b
    val streamingSizeLog = math.log(streamingSize) / math.log(2) // log2
    val powerComplexityFactor = math.pow(totalMult * streamingSize, 0.5) // sqrt

    // Return features in the EXACT same order as Python feature_columns
    Array(
      dataflowEncoded,
      totalMult,
      r,
      c,
      a,
      b,
      p,
      streamingSize,
      rCInteraction,
      aBPInteraction,
      rAPInteraction,
      cBPInteraction,
      rCABInteraction,
      streamingSizeLog,
      powerComplexityFactor
    )
  }

  def predict(features: InputFeatures): Try[ArraySynthesisData] = Try {
    val model = loadedModel.getOrElse(
      throw new IllegalStateException("Model not loaded. Call loadModel() first.")
    )

    // Engineer all features including interactions
    val engineeredFeatures = engineerFeatures(features)

    // Feature column names (must match Python exactly)
    val featureColumns = Array(
      "Dataflow_encoded",
      "Total Number of Multipliers",
      "R",
      "C",
      "A",
      "B",
      "P",
      "Streaming Dimension Size",
      "R_C_interaction",
      "A_B_P_interaction",
      "R_A_P_interaction",
      "C_B_P_interaction",
      "R_C_A_B_interaction",
      "Streaming_size_log",
      "Power_complexity_factor"
    )

    // Normalize features
    val normalizedAreaFeatures = normalizeFeatures(engineeredFeatures, model.areaFeatureStats, featureColumns)
    val normalizedPowerFeatures = normalizeFeatures(engineeredFeatures, model.powerFeatureStats, featureColumns)

    // Make predictions
    val areaPrediction = ensemblePredict(normalizedAreaFeatures, model.areaCoefficients)
    val powerPrediction = ensemblePredict(normalizedPowerFeatures, model.powerCoefficients)

    ArraySynthesisData(
      areaUm2 = areaPrediction,
      totalPowerMw = powerPrediction
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

  def debugPrediction(): Unit = {
    println("=== SCALA DEBUG PREDICTION WITH FEATURE ENGINEERING ===")

    val testFeatures = InputFeatures("OS", 1024, 32, 32, 1, 1, 1, 256)

    println(s"Input: ${testFeatures}")

    val model = loadedModel.getOrElse {
      println("ERROR: Model not loaded!")
      return
    }

    // Step 1: Feature engineering
    val engineeredFeatures = engineerFeatures(testFeatures)
    println(s"Step 1 - Engineered features: [${engineeredFeatures.mkString(", ")}]")

    // Step 2: Feature columns
    val featureColumns = Array(
      "Dataflow_encoded",
      "Total Number of Multipliers",
      "R",
      "C",
      "A",
      "B",
      "P",
      "Streaming Dimension Size",
      "R_C_interaction",
      "A_B_P_interaction",
      "R_A_P_interaction",
      "C_B_P_interaction",
      "R_C_A_B_interaction",
      "Streaming_size_log",
      "Power_complexity_factor"
    )

    println(s"Step 2 - Feature columns: [${featureColumns.mkString(", ")}]")

    // Step 3: Check area feature stats
    println("Step 3 - Area feature stats check:")
    featureColumns.zip(engineeredFeatures).foreach { case (col, value) =>
      model.areaFeatureStats.get(col) match {
        case Some(stats) =>
          val normalized = if (stats.range > 0) (value - stats.min) / stats.range else 0.0
          println(s"  ${col}: ${value} -> (${value} - ${stats.min}) / ${stats.range} = ${normalized}")
        case None =>
          println(s"  ${col}: NO STATS FOUND!")
      }
    }

    // Step 4: Check power feature stats
    println("Step 4 - Power feature stats check:")
    featureColumns.zip(engineeredFeatures).foreach { case (col, value) =>
      model.powerFeatureStats.get(col) match {
        case Some(stats) =>
          val normalized = if (stats.range > 0) (value - stats.min) / stats.range else 0.0
          println(s"  ${col}: ${value} -> (${value} - ${stats.min}) / ${stats.range} = ${normalized}")
        case None =>
          println(s"  ${col}: NO STATS FOUND!")
      }
    }

    // Step 5: Full prediction
    predict(testFeatures) match {
      case Success(result) =>
        println(s"Step 5 - Final predictions:")
        println(s"  Area: ${result.areaUm2}")
        println(s"  Power: ${result.totalPowerMw}")

      case Failure(ex) =>
        println(s"❌ Prediction failed: ${ex.getMessage}")
    }
  }
}
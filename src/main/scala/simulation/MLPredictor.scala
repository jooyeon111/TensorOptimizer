package simulation

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
 * A machine learning module for predicting array synthesis data (area and power)
 * based on the array configuration parameters.
 */
object MLPredictor extends Logger {

  case class ModelWeights(
                           // Linear regression weights for area prediction
                           areaIntercept: Double,
                           areaWeights: Map[String, Double],

                           // Linear regression weights for power prediction
                           powerIntercept: Double,
                           powerWeights: Map[String, Double]
                         )

  private case class FeatureVector(
                                    dataflow: String,
                                    totalMultipliers: Int,
                                    r: Int,
                                    c: Int,
                                    a: Int,
                                    b: Int,
                                    p: Int,
                                    streamingDimensionSize: Int
                                  ) {
    def toMap: Map[String, Double] = {
      // Create one-hot encoding for dataflow to avoid string features
      val dataflowFeatures = Map(
        "dataflow_Is" -> (if (dataflow == "IS") 1.0 else 0.0),
        "dataflow_Os" -> (if (dataflow == "OS") 1.0 else 0.0),
        "dataflow_Ws" -> (if (dataflow == "WS") 1.0 else 0.0)
      )

      // Ensure all values are valid numbers
      def safeDouble(value: Int): Double =
        if (value <= 0) 1.0 else value.toDouble  // Use 1.0 as a fallback for invalid values

      val basicFeatures = Map(
        "totalMultipliers" -> safeDouble(totalMultipliers),
        "r" -> safeDouble(r),
        "c" -> safeDouble(c),
        "a" -> safeDouble(a),
        "b" -> safeDouble(b),
        "p" -> safeDouble(p),
        "streamingDimensionSize" -> safeDouble(streamingDimensionSize)
      )

      // Feature transformations with safeguards
      val r_c = safeDouble(r) * safeDouble(c)
      val a_b = safeDouble(a) * safeDouble(b)
      val logMultipliers = math.log(safeDouble(totalMultipliers))
      val totalPe = safeDouble(r) * safeDouble(c) * safeDouble(a) * safeDouble(b)

      val derivedFeatures = Map(
        "r*c" -> r_c,
        "a*b" -> a_b,
        "log_totalMultipliers" -> logMultipliers,
        "total_pe" -> totalPe,

        // Add more useful features for the model
        "r*c*a*b" -> r_c * a_b,
        "r*a" -> safeDouble(r) * safeDouble(a),
        "c*b" -> safeDouble(c) * safeDouble(b),
        "r/c" -> safeDouble(r) / math.max(1.0, safeDouble(c)),
        "a/b" -> safeDouble(a) / math.max(1.0, safeDouble(b)),
        "streamingDimensionSize/totalMultipliers" -> safeDouble(streamingDimensionSize) / safeDouble(totalMultipliers)
      )

      // Combine all features
      dataflowFeatures ++ basicFeatures ++ derivedFeatures
    }
  }

  /**
   * Parse a CSV file and return the dataset with improved error handling
   */
  private def parseCSV(filePath: String): Try[Vector[(FeatureVector, (Double, Double))]] = {
    Try {
      val source = Source.fromResource(filePath)
      try {
        val lines = source.getLines().toVector
        if (lines.isEmpty) {
          throw new RuntimeException(s"CSV file $filePath is empty")
        }

        val header = lines.head.split(",").map(_.trim.replaceAll("\uFEFF", ""))
        println(s"CSV Header: ${header.mkString(",")}")

        // Find indices of each column
        val dataflowIndex = header.indexOf("Dataflow")
        println(s"Dataflow index: $dataflowIndex")
        if (dataflowIndex < 0) {
          throw new RuntimeException("Cannot find 'Dataflow' column in the CSV header")
        }

        val multiplierIndex = header.indexOf("Total Number of Multipliers")
        if (multiplierIndex < 0) {
          throw new RuntimeException("Cannot find 'Total Number of Multipliers' column in the CSV header")
        }

        val rIndex = header.indexOf("R")
        val cIndex = header.indexOf("C")
        val aIndex = header.indexOf("A")
        val bIndex = header.indexOf("B")
        val pIndex = header.indexOf("P")
        val streamingIndex = header.indexOf("Streaming Dimension Size")
        val areaIndex = header.indexOf("Area")
        val powerIndex = header.indexOf("Total Power")

        // Validate all required columns are present
        if (rIndex < 0 || cIndex < 0 || aIndex < 0 || bIndex < 0 || pIndex < 0 ||
          streamingIndex < 0 || areaIndex < 0 || powerIndex < 0) {
          throw new RuntimeException("Missing one or more required columns in the CSV header")
        }

        // Parse data rows with safeguards
        val result = lines.tail.flatMap { line =>
          try {
            val fields = line.split(",").map(_.trim)

            // Check if we have enough fields
            if (fields.length <= math.max(areaIndex, powerIndex)) {
              println(s"Warning: Skipping incomplete line: $line")
              None
            } else {
              // Check for non-numeric values before parsing
              val numericFields = Seq(multiplierIndex, rIndex, cIndex, aIndex, bIndex, pIndex, streamingIndex, areaIndex, powerIndex)
              val allNumeric = numericFields.forall(i => fields(i).matches("-?\\d+(\\.\\d+)?"))

              if (!allNumeric) {
                println(s"Warning: Skipping line with non-numeric values: $line")
                None
              } else {
                val features = FeatureVector(
                  dataflow = fields(dataflowIndex),
                  totalMultipliers = fields(multiplierIndex).toInt,
                  r = fields(rIndex).toInt,
                  c = fields(cIndex).toInt,
                  a = fields(aIndex).toInt,
                  b = fields(bIndex).toInt,
                  p = fields(pIndex).toInt,
                  streamingDimensionSize = fields(streamingIndex).toInt
                )

                val areaValue = fields(areaIndex).toDouble
                val powerValue = fields(powerIndex).toDouble

                // Check for valid numeric values
                if (areaValue <= 0 || powerValue <= 0 || areaValue.isNaN || powerValue.isNaN) {
                  println(s"Warning: Skipping line with invalid area/power values: $line")
                  None
                } else {
                  val targets = (areaValue, powerValue)
                  Some((features, targets))
                }
              }
            }
          } catch {
            case e: Exception =>
              println(s"Error parsing line: $line")
              println(s"Exception: ${e.getMessage}")
              None
          }
        }

        // Ensure we have some data
        if (result.isEmpty) {
          throw new RuntimeException(s"No valid data rows were parsed from $filePath")
        }

        println(s"Successfully parsed ${result.length} valid rows from CSV")
        result
      } finally {
        source.close()
      }
    }
  }

  /**
   * Train linear regression models for area and power prediction
   */
  private def trainLinearModels(
                                 trainData: Vector[(FeatureVector, (Double, Double))],
                                 validationData: Vector[(FeatureVector, (Double, Double))]
                               ): ModelWeights = {

    // Extract feature maps from dataset
    val trainFeatureMaps = trainData.map { case (features, _) => features.toMap }

    // Get all feature names
    val allFeatureNames = trainFeatureMaps.flatMap(_.keys).toSet.toVector

    // Train area model
    val (areaIntercept, areaWeights) = trainLinearModel(
      trainData.map { case (features, (area, _)) => (features.toMap, area) },
      validationData.map { case (features, (area, _)) => (features.toMap, area) },
      allFeatureNames
    )

    // Train power model
    val (powerIntercept, powerWeights) = trainLinearModel(
      trainData.map { case (features, (_, power)) => (features.toMap, power) },
      validationData.map { case (features, (_, power)) => (features.toMap, power) },
      allFeatureNames
    )

    ModelWeights(areaIntercept, areaWeights, powerIntercept, powerWeights)
  }

  /**
   * Train a linear regression model for a single target variable with improved stability
   */
  private def trainLinearModel(
                                trainData: Vector[(Map[String, Double], Double)],
                                validationData: Vector[(Map[String, Double], Double)],
                                allFeatureNames: Vector[String],
                                learningRate: Double = 0.001,  // Reduced from 0.01 to prevent large updates
                                l2Regularization: Double = 0.01,  // Reduced from 0.1 to prevent overfitting
                                epochs: Int = 1000
                              ): (Double, Map[String, Double]) = {

    // Initialize weights with small random values
    var intercept = 0.0
    var weights = allFeatureNames.map(feature => feature -> (math.random * 0.01 - 0.005)).toMap

    // Add gradient clipping values
    val gradientClipValue = 1.0

    // Simple gradient descent with safeguards
    for (epoch <- 1 to epochs) {
      var interceptGradient = 0.0
      var weightGradients = weights.map { case (feature, _) => feature -> 0.0 }

      // Add additional safeguard for empty data
      if (trainData.isEmpty) {
        println("Warning: Empty training data!")
        return (0.0, weights)
      }

      // Compute gradients for each sample
      for ((featureMap, target) <- trainData) {
        // Compute prediction with safeguards for numerical stability
        val prediction = intercept + featureMap.map { case (feature, value) =>
          weights.getOrElse(feature, 0.0) * value
        }.sum

        // Compute error with safeguard for NaN
        val error = prediction - target
        if (error.isNaN) {
          println(s"Warning: NaN error detected in epoch $epoch for target $target and prediction $prediction")
          // Skip this sample by using a conditional instead of 'continue'
        } else {
          // Only update gradients if error is valid
          // Update intercept gradient with safeguard
          interceptGradient += error

          // Update feature weight gradients with safeguards
          for ((feature, value) <- featureMap) {
            val gradientValue = error * value
            if (!gradientValue.isNaN && !gradientValue.isInfinite) {
              val currentGrad = weightGradients.getOrElse(feature, 0.0)
              weightGradients = weightGradients + (feature -> (currentGrad + gradientValue))
            }
          }
        }
      }

      // Average gradients
      val n = trainData.size.toDouble
      interceptGradient /= n
      weightGradients = weightGradients.map { case (feature, grad) => feature -> (grad / n) }

      // Apply L2 regularization to weights (not intercept) with safeguards
      weightGradients = weightGradients.map { case (feature, grad) =>
        val regularizationTerm = l2Regularization * weights.getOrElse(feature, 0.0)
        feature -> (grad + regularizationTerm)
      }

      // Apply gradient clipping to prevent extreme values
      interceptGradient = math.max(-gradientClipValue, math.min(gradientClipValue, interceptGradient))
      weightGradients = weightGradients.map { case (feature, grad) =>
        feature -> math.max(-gradientClipValue, math.min(gradientClipValue, grad))
      }

      // Update parameters with safeguards
      intercept -= learningRate * interceptGradient
      weights = weights.map { case (feature, weight) =>
        val updatedWeight = weight - learningRate * weightGradients.getOrElse(feature, 0.0)
        // Prevent NaN or infinite weights
        if (updatedWeight.isNaN || updatedWeight.isInfinite) {
          feature -> weight  // Keep the old weight if update is problematic
        } else {
          feature -> updatedWeight
        }
      }

      // Check validation error periodically with safeguards for NaN
      if (epoch % 100 == 0) {
        val trainRmse = computeRMSE(trainData, intercept, weights)
        val validationRmse = computeRMSE(validationData, intercept, weights)
        println(s"Epoch $epoch - Train RMSE: $trainRmse, Validation RMSE: $validationRmse")

        // Early stopping if we get NaN values (model diverged)
        if (trainRmse.isNaN || validationRmse.isNaN) {
          println("Warning: NaN values detected in RMSE. Adjusting learning rate and restarting.")
          return trainLinearModel(
            trainData,
            validationData,
            allFeatureNames,
            learningRate / 10.0,  // Reduce learning rate
            l2Regularization / 2.0,  // Reduce regularization
            epochs
          )
        }
      }
    }

    // Return the final model parameters
    (intercept, weights)
  }

  /**
   * Compute Root Mean Squared Error for model evaluation with safeguards for NaN
   */
  private def computeRMSE(
                           data: Vector[(Map[String, Double], Double)],
                           intercept: Double,
                           weights: Map[String, Double]
                         ): Double = {
    if (data.isEmpty) return 0.0  // Handle empty data case

    val squaredErrors = data.flatMap { case (featureMap, target) =>
      val prediction = intercept + featureMap.map { case (feature, value) =>
        weights.getOrElse(feature, 0.0) * value
      }.sum

      // Skip any NaN or Infinite predictions
      if (prediction.isNaN || prediction.isInfinite) {
        None
      } else {
        Some(math.pow(prediction - target, 2))
      }
    }

    if (squaredErrors.isEmpty) return Double.NaN  // If all predictions were NaN

    math.sqrt(squaredErrors.sum / squaredErrors.size)
  }

  /**
   * Compute Mean Absolute Error for model evaluation
   */
  private def computeMAE(
                          data: Vector[(Map[String, Double], Double)],
                          intercept: Double,
                          weights: Map[String, Double]
                        ): Double = {
    if (data.isEmpty) return 0.0

    val absoluteErrors = data.flatMap { case (featureMap, target) =>
      val prediction = intercept + featureMap.map { case (feature, value) =>
        weights.getOrElse(feature, 0.0) * value
      }.sum

      if (prediction.isNaN || prediction.isInfinite) {
        None
      } else {
        Some(math.abs(prediction - target))
      }
    }

    if (absoluteErrors.isEmpty) return Double.NaN

    absoluteErrors.sum / absoluteErrors.size
  }

  /**
   * Save model weights to a file
   */
  def saveModelWeights(weights: ModelWeights, filePath: String): Try[Unit] = {
    Try {
      val file = new File(filePath)
      val outputStream = new FileOutputStream(file)
      val objectOutputStream = new ObjectOutputStream(outputStream)

      try {
        objectOutputStream.writeObject(weights)
      } finally {
        objectOutputStream.close()
        outputStream.close()
      }
    }
  }

  /**
   * Load model weights from a file
   */
  def loadModelWeights(filePath: String): Try[ModelWeights] = {
    Try {
      val file = new File(filePath)
      val inputStream = new FileInputStream(file)
      val objectInputStream = new ObjectInputStream(inputStream)

      try {
        objectInputStream.readObject().asInstanceOf[ModelWeights]
      } finally {
        objectInputStream.close()
        inputStream.close()
      }
    }
  }

  /**
   * Predict array synthesis data using the trained model
   */
  def predictArraySynthesisData(
                                 dataflow: String,
                                 totalMultipliers: Int,
                                 groupPeRow: Int,
                                 groupPeCol: Int,
                                 vectorPeRow: Int,
                                 vectorPeCol: Int,
                                 numMultiplier: Int,
                                 streamingDimensionSize: Int,
                                 modelWeights: ModelWeights
                               ): ArraySynthesisData = {

    val features = FeatureVector(
      dataflow = dataflow,
      totalMultipliers = totalMultipliers,
      r = groupPeRow,
      c = groupPeCol,
      a = vectorPeRow,
      b = vectorPeCol,
      p = numMultiplier,
      streamingDimensionSize = streamingDimensionSize
    )

    val featureMap = features.toMap

    // Predict area
    val area = modelWeights.areaIntercept + featureMap.map { case (feature, value) =>
      modelWeights.areaWeights.getOrElse(feature, 0.0) * value
    }.sum

    // Predict total power
    val totalPower = modelWeights.powerIntercept + featureMap.map { case (feature, value) =>
      modelWeights.powerWeights.getOrElse(feature, 0.0) * value
    }.sum

    // Divide total power into its components (approximation)
    // Typical breakdown: switching (45%), internal (35%), leakage (20%)
    val switchPower = totalPower * 0.45
    val internalPower = totalPower * 0.35
    val leakagePower = totalPower * 0.20

    ArraySynthesisData(
      areaUm2 = math.max(area, 1.0),  // Ensure positive area
      switchPowerMw = math.max(switchPower, 0.001),  // Ensure positive power
      internalPowerMw = math.max(internalPower, 0.001),
      leakagePowerMw = math.max(leakagePower, 0.001)
    )
  }

  def trainModel(
                  weightOutputPath: String,
                  trainFilePath: String,
                  validationFilePath: String,
                  testFilePath: String,
                  loggerOption: LoggerOption
                ): Try[ModelWeights] = {
    setMode(loggerOption)
    log("Training a new model for array synthesis data prediction")

    try {
      // Parse data with proper error handling
      log(s"Loading training data from $trainFilePath")
      val trainDataResult = parseCSV(trainFilePath)
      val trainData = trainDataResult.getOrElse {
        log("Failed to parse training data. Check CSV format and contents.")
        return Failure(new RuntimeException("Failed to parse training data"))
      }

      log(s"Loading validation data from $validationFilePath")
      val validationDataResult = parseCSV(validationFilePath)
      val validationData = validationDataResult.getOrElse {
        log("Failed to parse validation data. Check CSV format and contents.")
        return Failure(new RuntimeException("Failed to parse validation data"))
      }

      log(s"Loading test data from $testFilePath")
      val testDataResult = parseCSV(testFilePath)
      val testData = testDataResult.getOrElse {
        log("Failed to parse test data. Check CSV format and contents.")
        return Failure(new RuntimeException("Failed to parse test data"))
      }

      // Log dataset statistics
      log(s"Training data size: ${trainData.size}")
      log(s"Validation data size: ${validationData.size}")
      log(s"Test data size: ${testData.size}")

      if (trainData.isEmpty) {
        log("Error: Training data is empty. Cannot train model.")
        return Failure(new RuntimeException("Empty training data"))
      }

      if (validationData.isEmpty) {
        log("Error: Validation data is empty. Cannot validate model.")
        return Failure(new RuntimeException("Empty validation data"))
      }

      // Extract and log feature statistics
      val firstSample = trainData.head._1
      val featureMap = firstSample.toMap
      log(s"Feature count: ${featureMap.size}")
      log(s"Feature examples: ${featureMap.keys.take(10).mkString(", ")}...")

      // Detect if numeric values are valid
      val hasInvalidValues = trainData.exists { case (features, (area, power)) =>
        val featureMap = features.toMap
        featureMap.values.exists(v => v.isNaN || v.isInfinite) ||
          area.isNaN || area.isInfinite || power.isNaN || power.isInfinite
      }

      if (hasInvalidValues) {
        log("Warning: Invalid values (NaN or Infinity) detected in dataset. Data preprocessing needed.")
      }

      log("Starting model training...")
      val modelWeights = trainLinearModels(trainData, validationData)
      log("Model training completed.")

      // Evaluate on test set if available
      if (testData.nonEmpty) {
        val areaTestData = testData.map { case (features, (area, _)) => (features.toMap, area) }
        val powerTestData = testData.map { case (features, (_, power)) => (features.toMap, power) }

        val areaRmse = computeRMSE(areaTestData, modelWeights.areaIntercept, modelWeights.areaWeights)
        val powerRmse = computeRMSE(powerTestData, modelWeights.powerIntercept, modelWeights.powerWeights)

        log(s"Test Area RMSE: $areaRmse")
        log(s"Test Power RMSE: $powerRmse")

        // Compute additional metrics for model evaluation
        val areaMae = computeMAE(areaTestData, modelWeights.areaIntercept, modelWeights.areaWeights)
        val powerMae = computeMAE(powerTestData, modelWeights.powerIntercept, modelWeights.powerWeights)

        log(s"Test Area MAE: $areaMae")
        log(s"Test Power MAE: $powerMae")

        // Log the top features by weight magnitude
        log("Top area features by importance:")
        modelWeights.areaWeights
          .toSeq
          .sortBy { case (_, weight) => -math.abs(weight) }
          .take(10)
          .foreach { case (feature, weight) => log(s"  $feature: $weight") }

        log("Top power features by importance:")
        modelWeights.powerWeights
          .toSeq
          .sortBy { case (_, weight) => -math.abs(weight) }
          .take(10)
          .foreach { case (feature, weight) => log(s"  $feature: $weight") }
      }

      // Save the model weights
      saveModelWeights(modelWeights, weightOutputPath) match {
        case Success(_) => log(s"Saved model weights to $weightOutputPath")
        case Failure(e) => log(s"Failed to save model weights: ${e.getMessage}")
      }

      Success(modelWeights)

    } catch {
      case e: Exception =>
        log(s"Failed to train model: ${e.getMessage}")
        e.printStackTrace()
        Failure(e)
    }
  }

  def loadModel(
                 weightFilePath: String,
                 loggerOption: LoggerOption
               ): Try[ModelWeights] = {
    setMode(loggerOption)
    log(s"Loading model weights from $weightFilePath")
    loadModelWeights(weightFilePath)
  }
}
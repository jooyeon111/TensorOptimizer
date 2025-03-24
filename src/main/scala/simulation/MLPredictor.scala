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

                           // Target normalization parameters for area
                           areaMean: Double,
                           areaStd: Double,

                           // Linear regression weights for power prediction
                           powerIntercept: Double,
                           powerWeights: Map[String, Double],

                           // Target normalization parameters for power
                           powerMean: Double,
                           powerStd: Double
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
   * Compute the mean and standard deviation for a set of values
   */
  private def computeStats(values: Vector[Double]): (Double, Double) = {
    val mean = values.sum / values.size
    val variance = values.map(x => math.pow(x - mean, 2)).sum / values.size
    val std = math.sqrt(variance)
    (mean, std)
  }

  /**
   * Normalize a vector of values using z-score normalization
   */
  private def normalizeValues(values: Vector[Double], mean: Double, std: Double): Vector[Double] = {
    values.map(v => (v - mean) / std)
  }

  /**
   * Train linear regression models for area and power prediction with target normalization
   */
  private def trainLinearModels(
                                 trainData: Vector[(FeatureVector, (Double, Double))],
                                 validationData: Vector[(FeatureVector, (Double, Double))]
                               ): ModelWeights = {

    // Extract feature maps from dataset
    val trainFeatureMaps = trainData.map { case (features, _) => features.toMap }

    // Get all feature names
    val allFeatureNames = trainFeatureMaps.flatMap(_.keys).toSet.toVector

    // Extract and normalize area values
    val trainAreaValues = trainData.map(_._2._1)
    val (areaMean, areaStd) = computeStats(trainAreaValues)
    val normalizedTrainAreaValues = normalizeValues(trainAreaValues, areaMean, areaStd)

    println(s"Area mean: $areaMean, std: $areaStd")

    // Extract and normalize power values
    val trainPowerValues = trainData.map(_._2._2)
    val (powerMean, powerStd) = computeStats(trainPowerValues)
    val normalizedTrainPowerValues = normalizeValues(trainPowerValues, powerMean, powerStd)

    println(s"Power mean: $powerMean, std: $powerStd")

    // Create normalized training data for area
    val normalizedTrainAreaData = trainData.zip(normalizedTrainAreaValues).map {
      case ((features, _), normalizedArea) => (features.toMap, normalizedArea)
    }

    // Create normalized validation data for area
    val validationAreaValues = validationData.map(_._2._1)
    val normalizedValidationAreaValues = normalizeValues(validationAreaValues, areaMean, areaStd)
    val normalizedValidationAreaData = validationData.zip(normalizedValidationAreaValues).map {
      case ((features, _), normalizedArea) => (features.toMap, normalizedArea)
    }

    // Create normalized training data for power
    val normalizedTrainPowerData = trainData.zip(normalizedTrainPowerValues).map {
      case ((features, _), normalizedPower) => (features.toMap, normalizedPower)
    }

    // Create normalized validation data for power
    val validationPowerValues = validationData.map(_._2._2)
    val normalizedValidationPowerValues = normalizeValues(validationPowerValues, powerMean, powerStd)
    val normalizedValidationPowerData = validationData.zip(normalizedValidationPowerValues).map {
      case ((features, _), normalizedPower) => (features.toMap, normalizedPower)
    }

    // Train area model with normalized targets
    val (areaIntercept, areaWeights) = trainLinearModel(
      normalizedTrainAreaData,
      normalizedValidationAreaData,
      allFeatureNames,
      learningRate = 0.001,
      l2Regularization = 0.01,
      epochs = 1000,
      modelName = "Area"
    )

    // Train power model with normalized targets
    val (powerIntercept, powerWeights) = trainLinearModel(
      normalizedTrainPowerData,
      normalizedValidationPowerData,
      allFeatureNames,
      learningRate = 0.001,
      l2Regularization = 0.01,
      epochs = 1000,
      modelName = "Power"
    )

    ModelWeights(
      areaIntercept, areaWeights, areaMean, areaStd,
      powerIntercept, powerWeights, powerMean, powerStd
    )
  }

  /**
   * Train a linear regression model for a single target variable with improved stability
   */
  private def trainLinearModel(
                                trainData: Vector[(Map[String, Double], Double)],
                                validationData: Vector[(Map[String, Double], Double)],
                                allFeatureNames: Vector[String],
                                learningRate: Double = 0.001,
                                l2Regularization: Double = 0.01,
                                epochs: Int = 1000,
                                modelName: String = "Generic"
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
          // Skip this sample
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
      if (epoch % 100 == 0 || epoch == epochs) {
        val trainRmse = computeRMSE(trainData, intercept, weights)
        val validationRmse = computeRMSE(validationData, intercept, weights)
        println(s"$modelName Model - Epoch $epoch - Train RMSE: $trainRmse, Validation RMSE: $validationRmse")

        // Early stopping if we get NaN values (model diverged)
        if (trainRmse.isNaN || validationRmse.isNaN) {
          println(s"Warning: NaN values detected in RMSE for $modelName model. Adjusting learning rate and restarting.")
          return trainLinearModel(
            trainData,
            validationData,
            allFeatureNames,
            learningRate / 10.0,  // Reduce learning rate
            l2Regularization / 2.0,  // Reduce regularization
            epochs,
            modelName
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
   * Compute Root Mean Squared Error for denormalized predictions
   */
  private def computeDenormalizedRMSE(
                                       data: Vector[(Map[String, Double], Double)],
                                       intercept: Double,
                                       weights: Map[String, Double],
                                       mean: Double,
                                       std: Double
                                     ): Double = {
    if (data.isEmpty) return 0.0

    val squaredErrors = data.flatMap { case (featureMap, target) =>
      // Get normalized prediction
      val normalizedPrediction = intercept + featureMap.map { case (feature, value) =>
        weights.getOrElse(feature, 0.0) * value
      }.sum

      // Denormalize prediction
      val prediction = normalizedPrediction * std + mean

      // Original target is already in original scale
      if (prediction.isNaN || prediction.isInfinite) {
        None
      } else {
        Some(math.pow(prediction - target, 2))
      }
    }

    if (squaredErrors.isEmpty) return Double.NaN

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
   * Compute Mean Absolute Error for denormalized predictions
   */
  private def computeDenormalizedMAE(
                                      data: Vector[(Map[String, Double], Double)],
                                      intercept: Double,
                                      weights: Map[String, Double],
                                      mean: Double,
                                      std: Double
                                    ): Double = {
    if (data.isEmpty) return 0.0

    val absoluteErrors = data.flatMap { case (featureMap, target) =>
      // Get normalized prediction
      val normalizedPrediction = intercept + featureMap.map { case (feature, value) =>
        weights.getOrElse(feature, 0.0) * value
      }.sum

      // Denormalize prediction
      val prediction = normalizedPrediction * std + mean

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

    // Get normalized area prediction
    val normalizedAreaPrediction = modelWeights.areaIntercept + featureMap.map { case (feature, value) =>
      modelWeights.areaWeights.getOrElse(feature, 0.0) * value
    }.sum

    // Denormalize prediction to get actual area
    val area = normalizedAreaPrediction * modelWeights.areaStd + modelWeights.areaMean

    // Get normalized power prediction
    val normalizedPowerPrediction = modelWeights.powerIntercept + featureMap.map { case (feature, value) =>
      modelWeights.powerWeights.getOrElse(feature, 0.0) * value
    }.sum

    // Denormalize prediction to get actual power
    val totalPower = normalizedPowerPrediction * modelWeights.powerStd + modelWeights.powerMean

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
        // Extract test data for evaluating original (non-normalized) performance
        val testAreaData = testData.map { case (features, (area, _)) => (features.toMap, area) }
        val testPowerData = testData.map { case (features, (_, power)) => (features.toMap, power) }

        // Evaluate normalized models on test data
        val testAreaRmse = computeDenormalizedRMSE(
          testAreaData,
          modelWeights.areaIntercept,
          modelWeights.areaWeights,
          modelWeights.areaMean,
          modelWeights.areaStd
        )

        val testPowerRmse = computeDenormalizedRMSE(
          testPowerData,
          modelWeights.powerIntercept,
          modelWeights.powerWeights,
          modelWeights.powerMean,
          modelWeights.powerStd
        )

        log(s"Test Area RMSE: $testAreaRmse")
        log(s"Test Power RMSE: $testPowerRmse")

        // Compute additional metrics for model evaluation
        val testAreaMae = computeDenormalizedMAE(
          testAreaData,
          modelWeights.areaIntercept,
          modelWeights.areaWeights,
          modelWeights.areaMean,
          modelWeights.areaStd
        )

        val testPowerMae = computeDenormalizedMAE(
          testPowerData,
          modelWeights.powerIntercept,
          modelWeights.powerWeights,
          modelWeights.powerMean,
          modelWeights.powerStd
        )

        log(s"Test Area MAE: $testAreaMae")
        log(s"Test Power MAE: $testPowerMae")

        // Calculate relative error (as percentage of average value)
        val averageArea = testAreaData.map(_._2).sum / testAreaData.size
        val relativAreaError = testAreaRmse / averageArea * 100
        log(s"Relative Area Error: ${relativAreaError.toInt}%")

        val averagePower = testPowerData.map(_._2).sum / testPowerData.size
        val relativePowerError = testPowerRmse / averagePower * 100
        log(s"Relative Power Error: ${relativePowerError.toInt}%")

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
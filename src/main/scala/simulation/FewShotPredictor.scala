package simulation

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import scala.io.Source
import scala.util.{Failure, Success, Try}
import scala.Array
import scala.collection.mutable.ArrayBuffer
import scala.math.{exp, pow, sqrt, abs, log}
import scala.util.Random

/**
 * FewShotPredictor provides a more robust prediction model for array synthesis data
 * by combining multiple prediction approaches:
 * 1. K-nearest neighbors for similar configurations
 * 2. Polynomial regression for better extrapolation to larger multiplier counts
 * 3. Feature selection to focus on most relevant features
 * 4. Ensemble techniques combining multiple models
 * 5. Regularization strategies to prevent overfitting
 */
object FewShotPredictor extends Logger {

  case class DataPoint(
    features: Map[String, Double],
    area: Double,
    power: Double
  ) extends Serializable

  case class FeatureVector(
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
      // Create one-hot encoding for dataflow
      val dataflowFeatures = Map(
        "dataflow_Is" -> (if (dataflow.toUpperCase == "IS") 1.0 else 0.0),
        "dataflow_Os" -> (if (dataflow.toUpperCase == "OS") 1.0 else 0.0),
        "dataflow_Ws" -> (if (dataflow.toUpperCase == "WS") 1.0 else 0.0)
      )

      // Ensure all values are valid numbers
      def safeDouble(value: Int): Double =
        if (value <= 0) 1.0 else value.toDouble  // Use 1.0 as a fallback

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
        "r*c*a*b" -> r_c * a_b,
        "r*a" -> safeDouble(r) * safeDouble(a),
        "c*b" -> safeDouble(c) * safeDouble(b),
        "r/c" -> safeDouble(r) / math.max(1.0, safeDouble(c)),
        "a/b" -> safeDouble(a) / math.max(1.0, safeDouble(b)),
        "streamingDimensionSize/totalMultipliers" -> safeDouble(streamingDimensionSize) / safeDouble(totalMultipliers),
        // Polynomial features
        "totalMultipliers^2" -> pow(safeDouble(totalMultipliers), 2),
        "totalMultipliers^3" -> pow(safeDouble(totalMultipliers), 3),
        "sqrt_totalMultipliers" -> sqrt(safeDouble(totalMultipliers))
      )

      // Combine all features
      dataflowFeatures ++ basicFeatures ++ derivedFeatures
    }
  }

  /**
   * Polynomial regression model for better extrapolation
   */
  case class PolynomialRegression(
    coefficientsArea: Map[String, Double],
    coefficientsPower: Map[String, Double],
    interceptArea: Double,
    interceptPower: Double,
    selectedFeatures: Array[String]
  ) extends Serializable

  /**
   * Combined model that contains both KNN and polynomial regression
   */
  case class FewShotModel(
    dataPoints: Array[DataPoint],
    featureImportance: Map[String, Double],
    featureMeans: Map[String, Double],
    featureStds: Map[String, Double],
    featureNames: Array[String],
    areaMean: Double,
    areaStd: Double,
    powerMean: Double,
    powerStd: Double,
    k: Int,
    distanceWeighting: Boolean,
    polynomialModel: PolynomialRegression,
    multiplierThreshold: Int = 20000  // Threshold to switch to polynomial model
  ) extends Serializable

  /**
   * Parses a CSV file containing array configurations and synthesis data
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

        // Find indices of each column
        val dataflowIndex = header.indexOf("Dataflow")
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
   * Calculate mean and standard deviation for a collection of values
   */
  private def computeStats(values: Vector[Double]): (Double, Double) = {
    val mean = values.sum / values.size
    val variance = values.map(x => math.pow(x - mean, 2)).sum / values.size
    val std = math.sqrt(variance)
    (mean, math.max(std, 0.0001)) // Avoid division by zero with minimum std
  }

  /**
   * Normalize features based on mean and standard deviation
   */
  private def normalizeFeatures(
    featureMap: Map[String, Double],
    means: Map[String, Double],
    stds: Map[String, Double]
  ): Map[String, Double] = {
    featureMap.map { case (feature, value) =>
      val mean = means.getOrElse(feature, 0.0)
      val std = stds.getOrElse(feature, 1.0)
      feature -> (if (std > 0.0001) (value - mean) / std else 0.0)
    }
  }

  /**
   * Calculate importance of each feature based on correlation with target values
   */
  private def calculateFeatureImportance(
    dataPoints: Array[DataPoint],
    featureNames: Array[String]
  ): Map[String, Double] = {
    val areaValues = dataPoints.map(_.area)
    val powerValues = dataPoints.map(_.power)

    // Compute importance as average of absolute correlation with area and power
    featureNames.map { feature =>
      val featureValues = dataPoints.map(_.features(feature))

      val areaCorrelation = computeCorrelation(featureValues, areaValues)
      val powerCorrelation = computeCorrelation(featureValues, powerValues)

      // Use average of absolute correlations as importance
      val importance = (math.abs(areaCorrelation) + math.abs(powerCorrelation)) / 2.0

      feature -> importance
    }.toMap
  }

  /**
   * Compute Pearson correlation coefficient between two vectors
   */
  private def computeCorrelation(x: Array[Double], y: Array[Double]): Double = {
    val n = x.length

    if (n != y.length || n < 2) {
      return 0.0 // Can't compute correlation with different sizes or too few points
    }

    val meanX = x.sum / n
    val meanY = y.sum / n

    val sumXY = x.zip(y).map { case (xi, yi) => (xi - meanX) * (yi - meanY) }.sum
    val sumX2 = x.map(xi => math.pow(xi - meanX, 2)).sum
    val sumY2 = y.map(yi => math.pow(yi - meanY, 2)).sum

    val denominator = math.sqrt(sumX2 * sumY2)

    if (denominator < 0.000001) {
      0.0 // Avoid division by zero
    } else {
      sumXY / denominator
    }
  }

  /**
   * Calculate weighted distance between two feature vectors, giving more weight to important features
   */
  private def weightedEuclideanDistance(
    features1: Map[String, Double],
    features2: Map[String, Double],
    featureImportance: Map[String, Double],
    commonFeatures: Set[String]
  ): Double = {
    val squaredDiffs = commonFeatures.map { feature =>
      val importance = featureImportance.getOrElse(feature, 1.0)
      val value1 = features1.getOrElse(feature, 0.0)
      val value2 = features2.getOrElse(feature, 0.0)
      importance * math.pow(value1 - value2, 2)
    }

    math.sqrt(squaredDiffs.sum)
  }

  /**
   * Find k nearest neighbors and predict area and power using weighted average
   */
  private def predictWithKnn(
    queryFeatures: Map[String, Double],
    dataPoints: Array[DataPoint],
    featureImportance: Map[String, Double],
    k: Int,
    distanceWeighting: Boolean
  ): (Double, Double) = {

    // Get common features between query and data points
    val commonFeatures = queryFeatures.keySet.intersect(
      if (dataPoints.nonEmpty) dataPoints.head.features.keySet else Set.empty
    )

    if (commonFeatures.isEmpty || dataPoints.isEmpty) {
      // Can't make a prediction without common features or data points
      return (0.0, 0.0)
    }

    // Calculate distance to all data points
    val distancesWithPoints = dataPoints.map { point =>
      val distance = weightedEuclideanDistance(
        queryFeatures, point.features, featureImportance, commonFeatures
      )
      (distance, point)
    }

    // Sort by distance and take k nearest
    val nearestNeighbors = distancesWithPoints.sortBy(_._1).take(k)

    if (nearestNeighbors.isEmpty) {
      return (0.0, 0.0)
    }

    // Calculate weighted predictions
    val (weightedAreaSum, weightedPowerSum, weightSum) = nearestNeighbors.foldLeft(
      (0.0, 0.0, 0.0)
    ) { case ((areaSum, powerSum, wSum), (distance, point)) =>
      // Calculate weight based on distance
      val weight = if (distanceWeighting) {
        if (distance < 0.000001) 1000.0 else 1.0 / (distance + 0.000001)
      } else {
        1.0 // Equal weights if distance weighting is disabled
      }

      (areaSum + weight * point.area, powerSum + weight * point.power, wSum + weight)
    }

    // Return weighted average
    val finalWeight = if (weightSum > 0.000001) weightSum else 1.0
    (weightedAreaSum / finalWeight, weightedPowerSum / finalWeight)
  }

  /**
   * Fit polynomial regression models for area and power prediction
   */
  private def fitPolynomialRegression(
    dataPoints: Array[DataPoint],
    selectedFeatures: Array[String]
  ): PolynomialRegression = {
    log("Fitting polynomial regression model...")

    // Extract data into matrices for solving least squares
    val n = dataPoints.length
    val m = selectedFeatures.length

    // Prepare the X matrix (feature values) and y vectors (targets)
    val X = Array.ofDim[Double](n, m + 1) // +1 for intercept
    val yArea = Array.ofDim[Double](n)
    val yPower = Array.ofDim[Double](n)

    // Fill the data matrices
    for (i <- 0 until n) {
      X(i)(0) = 1.0 // Intercept term
      for (j <- 0 until m) {
        X(i)(j + 1) = dataPoints(i).features.getOrElse(selectedFeatures(j), 0.0)
      }
      yArea(i) = dataPoints(i).area
      yPower(i) = dataPoints(i).power
    }

    // Solve for area coefficients using normal equations: (X^T X)^(-1) X^T y
    val coefficientsArea = solveNormalEquations(X, yArea)

    // Solve for power coefficients
    val coefficientsPower = solveNormalEquations(X, yPower)

    // Create coefficient maps
    val coeffMapArea = selectedFeatures.zipWithIndex.map { case (feature, i) =>
      feature -> coefficientsArea(i + 1) // +1 to skip intercept
    }.toMap

    val coeffMapPower = selectedFeatures.zipWithIndex.map { case (feature, i) =>
      feature -> coefficientsPower(i + 1) // +1 to skip intercept
    }.toMap

    PolynomialRegression(
      coefficientsArea = coeffMapArea,
      coefficientsPower = coeffMapPower,
      interceptArea = coefficientsArea(0),
      interceptPower = coefficientsPower(0),
      selectedFeatures = selectedFeatures
    )
  }

  /**
   * Solve normal equations for linear regression
   * Returns coefficients array where first element is intercept
   */
  private def solveNormalEquations(X: Array[Array[Double]], y: Array[Double]): Array[Double] = {
    val n = X.length
    val m = X(0).length

    // Compute X^T X
    val XTX = Array.ofDim[Double](m, m)
    for (i <- 0 until m) {
      for (j <- 0 until m) {
        var sum = 0.0
        for (k <- 0 until n) {
          sum += X(k)(i) * X(k)(j)
        }
        XTX(i)(j) = sum
      }
    }

    // Compute X^T y
    val XTy = Array.ofDim[Double](m)
    for (i <- 0 until m) {
      var sum = 0.0
      for (k <- 0 until n) {
        sum += X(k)(i) * y(k)
      }
      XTy(i) = sum
    }

    // Solve system using Gaussian elimination with regularization
    val regularization = 0.01 // L2 regularization factor
    for (i <- 0 until m) {
      XTX(i)(i) += regularization // Add regularization to diagonal
    }

    solveLinearSystem(XTX, XTy)
  }

  /**
   * Solve linear system Ax = b using Gaussian elimination with partial pivoting
   */
  private def solveLinearSystem(A: Array[Array[Double]], b: Array[Double]): Array[Double] = {
    val n = A.length
    val Aug = Array.ofDim[Double](n, n + 1)

    // Create augmented matrix [A|b]
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        Aug(i)(j) = A(i)(j)
      }
      Aug(i)(n) = b(i)
    }

    // Gaussian elimination with partial pivoting
    for (i <- 0 until n - 1) {
      // Find pivot
      var maxRow = i
      var maxVal = Math.abs(Aug(i)(i))

      for (k <- i + 1 until n) {
        if (Math.abs(Aug(k)(i)) > maxVal) {
          maxVal = Math.abs(Aug(k)(i))
          maxRow = k
        }
      }

      // Swap rows if necessary
      if (maxRow != i) {
        val temp = Aug(i)
        Aug(i) = Aug(maxRow)
        Aug(maxRow) = temp
      }

      // Eliminate below
      for (k <- i + 1 until n) {
        val factor = Aug(k)(i) / Aug(i)(i)
        for (j <- i until n + 1) {
          Aug(k)(j) -= factor * Aug(i)(j)
        }
      }
    }

    // Back substitution
    val x = Array.ofDim[Double](n)
    for (i <- n - 1 to 0 by -1) {
      var sum = 0.0
      for (j <- i + 1 until n) {
        sum += Aug(i)(j) * x(j)
      }
      x(i) = (Aug(i)(n) - sum) / Aug(i)(i)
    }

    x
  }

  /**
   * Predict using polynomial regression model
   */
  private def predictWithPolynomial(
    queryFeatures: Map[String, Double],
    model: PolynomialRegression
  ): (Double, Double) = {
    // Calculate area prediction
    var areaPrediction = model.interceptArea
    for (feature <- model.selectedFeatures) {
      val value = queryFeatures.getOrElse(feature, 0.0)
      areaPrediction += model.coefficientsArea.getOrElse(feature, 0.0) * value
    }

    // Calculate power prediction
    var powerPrediction = model.interceptPower
    for (feature <- model.selectedFeatures) {
      val value = queryFeatures.getOrElse(feature, 0.0)
      powerPrediction += model.coefficientsPower.getOrElse(feature, 0.0) * value
    }

    (areaPrediction, powerPrediction)
  }

  /**
   * Performs ensemble prediction by combining KNN and polynomial regression
   * For large multiplier counts, it gives more weight to polynomial regression
   */
  private def ensemblePrediction(
    queryFeatures: Map[String, Double],
    model: FewShotModel
  ): (Double, Double) = {
    val totalMultipliers = queryFeatures.getOrElse("totalMultipliers", 0.0)

    // Calculate weight for polynomial model based on multiplier count
    val polyWeight = if (totalMultipliers > model.multiplierThreshold) {
      // For very large multiplier counts, rely more on polynomial regression
      val multiplierRatio = Math.min(1.0, (totalMultipliers - model.multiplierThreshold) / model.multiplierThreshold)
      0.5 + 0.5 * multiplierRatio
    } else {
      // For smaller multiplier counts, rely more on KNN
      0.3
    }

    // Get KNN prediction
    val (knnArea, knnPower) = predictWithKnn(
      queryFeatures, model.dataPoints, model.featureImportance, model.k, model.distanceWeighting
    )

    // Get polynomial prediction
    val (polyArea, polyPower) = predictWithPolynomial(queryFeatures, model.polynomialModel)

    // Combine predictions with weighted average
    val areaPred = polyWeight * polyArea + (1 - polyWeight) * knnArea
    val powerPred = polyWeight * polyPower + (1 - polyWeight) * knnPower

    (areaPred, powerPred)
  }

  /**
   * Save model to a file
   */
  def saveModel(model: FewShotModel, filePath: String): Try[Unit] = {
    Try {
      val file = new File(filePath)
      val outputStream = new FileOutputStream(file)
      val objectOutputStream = new ObjectOutputStream(outputStream)

      try {
        objectOutputStream.writeObject(model)
      } finally {
        objectOutputStream.close()
        outputStream.close()
      }
    }
  }

  /**
   * Load model from a file
   */
  def loadModel(filePath: String, loggerOption: LoggerOption = null): Try[FewShotModel] = {
    if (loggerOption != null) {
      setMode(loggerOption)
    }

    Try {
      println(s"$filePath")
      val file = new File("src/main/resources/" + filePath)
      val inputStream = new FileInputStream(file)
      val objectInputStream = new ObjectInputStream(inputStream)

      try {
        objectInputStream.readObject().asInstanceOf[FewShotModel]
      } finally {
        objectInputStream.close()
        inputStream.close()
      }
    }
  }

  /**
   * Train a model using the provided training data
   */
  def trainModel(
    weightOutputPath: String,
    trainFilePath: String,
    validationFilePath: String,
    testFilePath: String,
    loggerOption: LoggerOption
  ): Try[FewShotModel] = {
    setMode(loggerOption)
    log("Training a new FewShotPredictor model for array synthesis data prediction")

    try {
      // Parse data
      log(s"Loading training data from $trainFilePath")
      val trainDataResult = parseCSV(trainFilePath)
      val trainData = trainDataResult.getOrElse {
        log("Failed to parse training data.")
        return Failure(new RuntimeException("Failed to parse training data"))
      }

      log(s"Loading validation data from $validationFilePath")
      val validationDataResult = parseCSV(validationFilePath)
      val validationData = validationDataResult.getOrElse {
        log("Failed to parse validation data.")
        return Failure(new RuntimeException("Failed to parse validation data"))
      }

      log(s"Loading test data from $testFilePath")
      val testDataResult = parseCSV(testFilePath)
      val testData = testDataResult.getOrElse {
        log("Failed to parse test data.")
        return Failure(new RuntimeException("Failed to parse test data"))
      }

      // Extract feature names from the first sample
      val allFeatureNames = trainData.head._1.toMap.keys.toArray
      log(s"Features (${allFeatureNames.length}): ${allFeatureNames.mkString(", ")}")

      // Calculate feature statistics for normalization
      val allFeatureMaps = trainData.map(_._1.toMap)
      val featureMeans = scala.collection.mutable.Map[String, Double]()
      val featureStds = scala.collection.mutable.Map[String, Double]()

      for (feature <- allFeatureNames) {
        val values = allFeatureMaps.map(_.getOrElse(feature, 0.0))
        val (mean, std) = computeStats(values)
        featureMeans(feature) = mean
        featureStds(feature) = std
      }

      // Extract and normalize area and power values
      val trainAreaValues = trainData.map(_._2._1)
      val trainPowerValues = trainData.map(_._2._2)

      val (areaMean, areaStd) = computeStats(trainAreaValues)
      val (powerMean, powerStd) = computeStats(trainPowerValues)

      log(s"Area stats: mean=$areaMean, std=$areaStd")
      log(s"Power stats: mean=$powerMean, std=$powerStd")

      // Create normalized data points
      val normalizedDataPoints = trainData.map { case (features, (area, power)) =>
        val normalizedFeatures = normalizeFeatures(features.toMap, featureMeans.toMap, featureStds.toMap)
        val normalizedArea = (area - areaMean) / areaStd
        val normalizedPower = (power - powerMean) / powerStd

        DataPoint(normalizedFeatures, normalizedArea, normalizedPower)
      }.toArray

      // Calculate feature importance
      log("Calculating feature importance...")
      val featureImportance = calculateFeatureImportance(normalizedDataPoints, allFeatureNames)

      // Print top features by importance
      val topFeatures = featureImportance.toSeq.sortBy(-_._2).take(10)
      log("Top 10 most important features:")
      topFeatures.foreach { case (feature, importance) =>
        log(f"$feature: $importance%.4f")
      }

      // Choose optimal k using validation data
      log("Selecting optimal k for k-nearest neighbors...")
      val kCandidates = Vector(3, 5, 7, 9, 11, 15, 19)
      val distanceWeightingOptions = Vector(true, false)

      var bestValidationError = Double.MaxValue
      var bestK = 5 // Default
      var bestDistanceWeighting = true // Default

      for {
        k <- kCandidates
        useDistanceWeighting <- distanceWeightingOptions
      } {
        val validationErrors = validationData.map { case (features, (actualArea, actualPower)) =>
          val normalizedFeatures = normalizeFeatures(features.toMap, featureMeans.toMap, featureStds.toMap)
          val (predNormalizedArea, predNormalizedPower) = predictWithKnn(
            normalizedFeatures, normalizedDataPoints, featureImportance, k, useDistanceWeighting
          )

          // Denormalize predictions
          val predArea = predNormalizedArea * areaStd + areaMean
          val predPower = predNormalizedPower * powerStd + powerMean

          // Calculate relative error
          val areaError = math.abs(predArea - actualArea) / actualArea
          val powerError = math.abs(predPower - actualPower) / actualPower

          // Combined error metric
          (areaError + powerError) / 2.0
        }

        val avgError = validationErrors.sum / validationErrors.length
        log(f"k=$k, useDistanceWeighting=$useDistanceWeighting: Average validation error=$avgError%.4f")

        if (avgError < bestValidationError) {
          bestValidationError = avgError
          bestK = k
          bestDistanceWeighting = useDistanceWeighting
        }
      }

      log(f"Selected optimal parameters: k=$bestK, useDistanceWeighting=$bestDistanceWeighting")

      // Select important features for polynomial regression
      // We'll focus on features most related to the multiplier count and dataflow
      val polynomialFeatures = Array(
        "totalMultipliers", "totalMultipliers^2", "totalMultipliers^3", "sqrt_totalMultipliers",
        "r*c", "a*b", "r*c*a*b", "log_totalMultipliers", "total_pe",
        "dataflow_Is", "dataflow_Os", "dataflow_Ws",
        "streamingDimensionSize"
      ).filter(f => allFeatureNames.contains(f))

      // Train polynomial regression model
      val polynomialModel = fitPolynomialRegression(normalizedDataPoints, polynomialFeatures)

      // Test the polynomial model on validation data
      log("Testing polynomial regression model...")
      val polyValidationErrors = validationData.map { case (features, (actualArea, actualPower)) =>
        val normalizedFeatures = normalizeFeatures(features.toMap, featureMeans.toMap, featureStds.toMap)
        val (predNormalizedArea, predNormalizedPower) = predictWithPolynomial(normalizedFeatures, polynomialModel)

        // Denormalize predictions
        val predArea = predNormalizedArea * areaStd + areaMean
        val predPower = predNormalizedPower * powerStd + powerMean

        // Calculate relative error
        val areaError = math.abs(predArea - actualArea) / actualArea
        val powerError = math.abs(predPower - actualPower) / actualPower

        // Combined error metric
        (areaError + powerError) / 2.0
      }

      val avgPolyError = polyValidationErrors.sum / polyValidationErrors.length
      log(f"Polynomial regression validation error: $avgPolyError%.4f")

      // Determine threshold for switching to polynomial model
      // Set the threshold based on the largest multiplier count in the training data
      val maxMultiplierCount = trainData.map(_._1.totalMultipliers).max
      val multiplierThreshold = maxMultiplierCount / 2

      // Create the final model
      val model = FewShotModel(
        dataPoints = normalizedDataPoints,
        featureImportance = featureImportance,
        featureMeans = featureMeans.toMap,
        featureStds = featureStds.toMap,
        featureNames = allFeatureNames,
        areaMean = areaMean,
        areaStd = areaStd,
        powerMean = powerMean,
        powerStd = powerStd,
        k = bestK,
        distanceWeighting = bestDistanceWeighting,
        polynomialModel = polynomialModel,
        multiplierThreshold = multiplierThreshold
      )

      // Evaluate the model using test data
      evaluateModel(model, testData)

      // Save the model
      saveModel(model, weightOutputPath) match {
        case Success(_) => log(s"Saved model to $weightOutputPath")
        case Failure(e) => log(s"Failed to save model: ${e.getMessage}")
      }

      Success(model)
    } catch {
      case e: Exception =>
        log(s"Failed to train model: ${e.getMessage}")
        e.printStackTrace()
        Failure(e)
    }
  }

  /**
   * Evaluate a model using test data
   */
  def evaluateModel(model: FewShotModel, testData: Vector[(FeatureVector, (Double, Double))]): Unit = {
    log("Evaluating model on test data...")

    // Evaluate standard KNN predictions
    val knnPredictions = testData.map { case (features, (actualArea, actualPower)) =>
      val normalizedFeatures = normalizeFeatures(
        features.toMap, model.featureMeans, model.featureStds
      )

      val (predNormalizedArea, predNormalizedPower) = predictWithKnn(
        normalizedFeatures, model.dataPoints, model.featureImportance, model.k, model.distanceWeighting
      )

      // Denormalize predictions
      val predArea = predNormalizedArea * model.areaStd + model.areaMean
      val predPower = predNormalizedPower * model.powerStd + model.powerMean

      ((predArea, predPower), (actualArea, actualPower), features.totalMultipliers)
    }

    // Evaluate polynomial model predictions
    val polyPredictions = testData.map { case (features, (actualArea, actualPower)) =>
      val normalizedFeatures = normalizeFeatures(
        features.toMap, model.featureMeans, model.featureStds
      )

      val (predNormalizedArea, predNormalizedPower) = predictWithPolynomial(
        normalizedFeatures, model.polynomialModel
      )

      // Denormalize predictions
      val predArea = predNormalizedArea * model.areaStd + model.areaMean
      val predPower = predNormalizedPower * model.powerStd + model.powerMean

      ((predArea, predPower), (actualArea, actualPower), features.totalMultipliers)
    }

    // Evaluate ensemble predictions
    val ensemblePredictions = testData.map { case (features, (actualArea, actualPower)) =>
      val normalizedFeatures = normalizeFeatures(
        features.toMap, model.featureMeans, model.featureStds
      )

      val (predNormalizedArea, predNormalizedPower) = ensemblePrediction(
        normalizedFeatures, model
      )

      // Denormalize predictions
      val predArea = predNormalizedArea * model.areaStd + model.areaMean
      val predPower = predNormalizedPower * model.powerStd + model.powerMean

      ((predArea, predPower), (actualArea, actualPower), features.totalMultipliers)
    }

    // Calculate error metrics for KNN
    val knnAreaErrors = knnPredictions.map { case ((predArea, _), (actualArea, _), _) =>
      math.abs(predArea - actualArea) / actualArea
    }

    val knnPowerErrors = knnPredictions.map { case ((_, predPower), (_, actualPower), _) =>
      math.abs(predPower - actualPower) / actualPower
    }

    val knnMeanAreaError = knnAreaErrors.sum / knnAreaErrors.length
    val knnMeanPowerError = knnPowerErrors.sum / knnPowerErrors.length

    // Calculate error metrics for polynomial model
    val polyAreaErrors = polyPredictions.map { case ((predArea, _), (actualArea, _), _) =>
      math.abs(predArea - actualArea) / actualArea
    }

    val polyPowerErrors = polyPredictions.map { case ((_, predPower), (_, actualPower), _) =>
      math.abs(predPower - actualPower) / actualPower
    }

    val polyMeanAreaError = polyAreaErrors.sum / polyAreaErrors.length
    val polyMeanPowerError = polyPowerErrors.sum / polyPowerErrors.length

    // Calculate error metrics for ensemble model
    val ensembleAreaErrors = ensemblePredictions.map { case ((predArea, _), (actualArea, _), _) =>
      math.abs(predArea - actualArea) / actualArea
    }

    val ensemblePowerErrors = ensemblePredictions.map { case ((_, predPower), (_, actualPower), _) =>
      math.abs(predPower - actualPower) / actualPower
    }

    val ensembleMeanAreaError = ensembleAreaErrors.sum / ensembleAreaErrors.length
    val ensembleMeanPowerError = ensemblePowerErrors.sum / ensemblePowerErrors.length

    log(f"KNN Mean Area Percentage Error: ${knnMeanAreaError * 100}%.2f%%")
    log(f"KNN Mean Power Percentage Error: ${knnMeanPowerError * 100}%.2f%%")
    log(f"Polynomial Mean Area Percentage Error: ${polyMeanAreaError * 100}%.2f%%")
    log(f"Polynomial Mean Power Percentage Error: ${polyMeanPowerError * 100}%.2f%%")
    log(f"Ensemble Mean Area Percentage Error: ${ensembleMeanAreaError * 100}%.2f%%")
    log(f"Ensemble Mean Power Percentage Error: ${ensembleMeanPowerError * 100}%.2f%%")

    // Calculate RMSE for ensemble model
    val areaRMSE = math.sqrt(
      ensemblePredictions.map { case ((predArea, _), (actualArea, _), _) =>
        math.pow(predArea - actualArea, 2)
      }.sum / ensemblePredictions.length
    )

    val powerRMSE = math.sqrt(
      ensemblePredictions.map { case ((_, predPower), (_, actualPower), _) =>
        math.pow(predPower - actualPower, 2)
      }.sum / ensemblePredictions.length
    )

    log(f"Ensemble Area RMSE: $areaRMSE%.2f")
    log(f"Ensemble Power RMSE: $powerRMSE%.2f")

    // Performance analysis for different multiplier ranges
    val multiplierRanges = Vector(
      (0, 5000, "Small (0-5K)"),
      (5000, 20000, "Medium (5K-20K)"),
      (20000, Int.MaxValue, "Large (>20K)")
    )

    log("\nPerformance by multiplier range:")

    multiplierRanges.foreach { case (min, max, label) =>
      val rangeData = ensemblePredictions.filter { case (_, _, multipliers) =>
        multipliers >= min && multipliers < max
      }

      if (rangeData.nonEmpty) {
        val rangeAreaErrors = rangeData.map { case ((predArea, _), (actualArea, _), _) =>
          math.abs(predArea - actualArea) / actualArea
        }

        val rangePowerErrors = rangeData.map { case ((_, predPower), (_, actualPower), _) =>
          math.abs(predPower - actualPower) / actualPower
        }

        val rangeMeanAreaError = rangeAreaErrors.sum / rangeAreaErrors.length
        val rangeMeanPowerError = rangePowerErrors.sum / rangePowerErrors.length

        log(f"$label multipliers (${rangeData.length} samples):")
        log(f"  Area Error: ${rangeMeanAreaError * 100}%.2f%%")
        log(f"  Power Error: ${rangeMeanPowerError * 100}%.2f%%")
      } else {
        log(f"$label multipliers: No samples")
      }
    }

    // Show examples
    log("\nSample predictions (Predicted vs Actual):")
    ensemblePredictions.sortBy(_._3).reverse.take(5).foreach { case ((predArea, predPower), (actualArea, actualPower), multipliers) =>
      log(f"Multipliers: $multipliers, " +
        f"Area: $predArea%.2f vs $actualArea%.2f (${math.abs(predArea - actualArea) / actualArea * 100}%.2f%%), " +
        f"Power: $predPower%.2f vs $actualPower%.2f (${math.abs(predPower - actualPower) / actualPower * 100}%.2f%%)")
    }
  }

  /**
   * Predict area and power for a given configuration
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
                                 model: FewShotModel
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

    // Convert to feature map and normalize
    val normalizedFeatures = normalizeFeatures(
      features.toMap, model.featureMeans, model.featureStds
    )

    // Use ensemble prediction for better accuracy across all multiplier ranges
    val (normalizedAreaPred, normalizedPowerPred) = ensemblePrediction(
      normalizedFeatures, model
    )

    // Denormalize predictions
    val areaPrediction = normalizedAreaPred * model.areaStd + model.areaMean
    val powerPrediction = normalizedPowerPred * model.powerStd + model.powerMean

    // Ensure predictions are positive
    val area = math.max(areaPrediction, 1.0)
    val totalPower = math.max(powerPrediction, 0.001)

    // Divide total power into components
    val switchPower = totalPower * 0.45
    val internalPower = totalPower * 0.35
    val leakagePower = totalPower * 0.20

    ArraySynthesisData(
      areaUm2 = area,
      switchPowerMw = switchPower,
      internalPowerMw = internalPower,
      leakagePowerMw = leakagePower
    )
  }
}
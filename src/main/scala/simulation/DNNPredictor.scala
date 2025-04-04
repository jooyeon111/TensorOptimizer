package simulation

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import scala.io.Source
import scala.Array
import scala.util.{Failure, Success, Try}
import scala.collection.mutable.ArrayBuffer
import scala.math.{exp, pow, sqrt, abs, log}
import scala.util.Random

/**
 * A deep neural network module for predicting array synthesis data (area and power)
 * based on the array configuration parameters.
 */
object DNNPredictor extends Logger {

  /**
   * Class to represent a neural network layer
   */
  case class Layer(
    weights: Array[Array[Double]],
    biases: Array[Double],
    activation: String // "relu", "linear", or "sigmoid"
  ) extends Serializable

  /**
   * Deep Neural Network model containing multiple layers
   */
  case class DNNModel(
    layers: Array[Layer],

    // Normalization parameters for inputs
    featureMeans: Map[String, Double],
    featureStds: Map[String, Double],

    // Normalization parameters for targets
    areaMean: Double,
    areaStd: Double,
    powerMean: Double,
    powerStd: Double,

    // Feature names to ensure consistent ordering
    featureNames: Array[String]
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
   * Compute the mean and standard deviation for a set of values
   */
  private def computeStats(values: Vector[Double]): (Double, Double) = {
    val mean = values.sum / values.size
    val variance = values.map(x => math.pow(x - mean, 2)).sum / values.size
    val std = math.sqrt(variance)
    (mean, std)
  }

  /**
   * Normalize a feature map using z-score normalization
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
   * Normalize a vector of values using z-score normalization
   */
  private def normalizeValues(values: Vector[Double], mean: Double, std: Double): Vector[Double] = {
    values.map(v => (v - mean) / std)
  }

  /**
   * Create a neural network with specified architecture
   */
  private def createNeuralNetwork(
                                   inputSize: Int,
                                   hiddenLayers: Array[Int],
                                   outputSize: Int,
                                   seed: Int = 42
                                 ): Array[Layer] = {
    val random = new Random(seed)

    val allLayerSizes = Array(inputSize) ++ hiddenLayers ++ Array(outputSize)

    val layers = for (i <- 0 until allLayerSizes.length - 1) yield {
      val inSize = allLayerSizes(i)
      val outSize = allLayerSizes(i + 1)

      // Xavier/Glorot initialization for weights
      val scale = sqrt(6.0 / (inSize + outSize))

      val weights = Array.fill(outSize, inSize) {
        random.nextDouble() * 2 * scale - scale
      }

      val biases = Array.fill(outSize)(0.0)

      // Use ReLU for hidden layers, linear for output layer
      val activation = if (i < allLayerSizes.length - 2) "relu" else "linear"

      Layer(weights, biases, activation)
    }

    layers.toArray
  }

  /**
   * ReLU activation function
   */
  private def relu(x: Double): Double = math.max(0.0, x)

  /**
   * Derivative of ReLU
   */
  private def reluDerivative(x: Double): Double = if (x > 0) 1.0 else 0.0

  /**
   * Sigmoid activation function
   */
  private def sigmoid(x: Double): Double = 1.0 / (1.0 + math.exp(-x))

  /**
   * Derivative of sigmoid
   */
  private def sigmoidDerivative(x: Double): Double = {
    val s = sigmoid(x)
    s * (1 - s)
  }

  /**
   * Apply activation function based on layer type
   */
  private def applyActivation(x: Double, activationType: String): Double = {
    activationType match {
      case "relu" => relu(x)
      case "sigmoid" => sigmoid(x)
      case "linear" => x
      case _ => throw new IllegalArgumentException(s"Unsupported activation type: $activationType")
    }
  }

  /**
   * Get activation function derivative
   */
  private def activationDerivative(x: Double, activationType: String): Double = {
    activationType match {
      case "relu" => reluDerivative(x)
      case "sigmoid" => sigmoidDerivative(x)
      case "linear" => 1.0
      case _ => throw new IllegalArgumentException(s"Unsupported activation type: $activationType")
    }
  }

  /**
   * Forward pass through the neural network
   */
  private def forwardPass(
                           inputs: Array[Double],
                           layers: Array[Layer]
                         ): (Array[Array[Double]], Array[Array[Double]]) = {

    val activations = new Array[Array[Double]](layers.length + 1)
    val preActivations = new Array[Array[Double]](layers.length)

    activations(0) = inputs

    for (i <- layers.indices) {
      val layer = layers(i)
      val prevActivation = activations(i)

      val preActivation = new Array[Double](layer.biases.length)
      val activation = new Array[Double](layer.biases.length)

      for (j <- layer.biases.indices) {
        var sum = layer.biases(j)
        for (k <- prevActivation.indices) {
          sum += layer.weights(j)(k) * prevActivation(k)
        }
        preActivation(j) = sum
        activation(j) = applyActivation(sum, layer.activation)
      }

      preActivations(i) = preActivation
      activations(i + 1) = activation
    }

    (activations, preActivations)
  }

  /**
   * Backpropagation to compute gradients
   */
  private def backpropagation(
                               inputs: Array[Double],
                               target: Array[Double],
                               layers: Array[Layer],
                               l2Lambda: Double = 0.001
                             ): (Array[Array[Array[Double]]], Array[Array[Double]]) = {

    val (activations, preActivations) = forwardPass(inputs, layers)

    // Initialize the gradient arrays with the correct sizes
    val weightGradients = new Array[Array[Array[Double]]](layers.length)
    val biasGradients = new Array[Array[Double]](layers.length)

    for (i <- layers.indices) {
      weightGradients(i) = Array.ofDim[Double](layers(i).weights.length, layers(i).weights(0).length)
      biasGradients(i) = Array.ofDim[Double](layers(i).biases.length)
    }

    // Calculate output layer error
    val outputLayerIdx = layers.length - 1
    val outputErrors = new Array[Double](layers(outputLayerIdx).biases.length)

    for (i <- outputErrors.indices) {
      val error = activations(outputLayerIdx + 1)(i) - target(i)
      outputErrors(i) = error
    }

    // Backpropagate error
    var nextErrors = outputErrors

    for (layerIdx <- (0 until layers.length).reverse) {
      val layer = layers(layerIdx)
      val layerInputs = activations(layerIdx)
      val preActivation = preActivations(layerIdx)

      val currentErrors = if (layerIdx == outputLayerIdx) {
        // For output layer, use the calculated output errors
        nextErrors
      } else {
        // For hidden layers, backpropagate errors from next layer
        val nextLayer = layers(layerIdx + 1)
        val errors = new Array[Double](layer.biases.length)

        for (i <- errors.indices) {
          var error = 0.0
          for (j <- nextErrors.indices) {
            error += nextLayer.weights(j)(i) * nextErrors(j)
          }
          errors(i) = error * activationDerivative(preActivation(i), layer.activation)
        }
        errors
      }

      // Calculate gradients
      for (i <- currentErrors.indices) {
        biasGradients(layerIdx)(i) = currentErrors(i)

        for (j <- layerInputs.indices) {
          // L2 regularization gradient component
          val regTerm = l2Lambda * layer.weights(i)(j)
          weightGradients(layerIdx)(i)(j) = currentErrors(i) * layerInputs(j) + regTerm
        }
      }

      nextErrors = currentErrors
    }

    (weightGradients, biasGradients)
  }

  /**
   * Update network parameters using calculated gradients
   */
  private def updateParameters(
                                layers: Array[Layer],
                                weightGradients: Array[Array[Array[Double]]],
                                biasGradients: Array[Array[Double]],
                                learningRate: Double,
                                clipValue: Double = 5.0
                              ): Array[Layer] = {

    val updatedLayers = new Array[Layer](layers.length)

    for (i <- layers.indices) {
      val layer = layers(i)
      val wGrads = weightGradients(i)
      val bGrads = biasGradients(i)

      // Create new weights and biases arrays
      val newWeights = Array.ofDim[Double](layer.weights.length, layer.weights(0).length)
      val newBiases = new Array[Double](layer.biases.length)

      // Update weights with gradient clipping
      for (j <- layer.weights.indices) {
        for (k <- layer.weights(0).indices) {
          val clippedGrad = math.max(-clipValue, math.min(clipValue, wGrads(j)(k)))
          newWeights(j)(k) = layer.weights(j)(k) - learningRate * clippedGrad
        }

        // Update biases with gradient clipping
        val clippedBiasGrad = math.max(-clipValue, math.min(clipValue, bGrads(j)))
        newBiases(j) = layer.biases(j) - learningRate * clippedBiasGrad
      }

      updatedLayers(i) = Layer(newWeights, newBiases, layer.activation)
    }

    updatedLayers
  }

  /**
   * Mean Squared Error loss function
   */
  private def mseError(predictions: Array[Double], targets: Array[Double]): Double = {
    var loss = 0.0
    for (i <- predictions.indices) {
      loss += math.pow(predictions(i) - targets(i), 2)
    }
    loss / predictions.length
  }

  /**
   * Predict using the neural network model
   */
  private def predict(
                       inputs: Array[Double],
                       layers: Array[Layer]
                     ): Array[Double] = {
    val (activations, _) = forwardPass(inputs, layers)
    activations.last
  }

  /**
   * Train a neural network model for area and power prediction
   */
  private def trainNeuralNetworks(
    trainData: Vector[(FeatureVector, (Double, Double))],
    validationData: Vector[(FeatureVector, (Double, Double))],
    learningRate: Double = 0.001,
    batchSize: Int = 32,
    epochs: Int = 1000,
    patience: Int = 50, // For early stopping
    l2Lambda: Double = 0.001
  ): DNNModel = {
    log("Starting neural network training...")

    // Extract all feature names from the first sample
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

    // Extract and normalize area values
    val trainAreaValues = trainData.map(_._2._1)
    val (areaMean, areaStd) = computeStats(trainAreaValues)
    val normalizedTrainAreaValues = normalizeValues(trainAreaValues, areaMean, areaStd)

    // Extract and normalize power values
    val trainPowerValues = trainData.map(_._2._2)
    val (powerMean, powerStd) = computeStats(trainPowerValues)
    val normalizedTrainPowerValues = normalizeValues(trainPowerValues, powerMean, powerStd)

    log(s"Area stats: mean=$areaMean, std=$areaStd")
    log(s"Power stats: mean=$powerMean, std=$powerStd")

    // Create a single neural network for both area and power prediction
    val inputSize = allFeatureNames.length
    val outputSize = 2 // Area and power

    val hiddenLayers = Array(128, 96, 64, 48, 32, 24, 16, 8)

    var network = createNeuralNetwork(inputSize, hiddenLayers, outputSize)

    // Prepare training data
    val normalizedTrainInputs = trainData.map { case (features, _) =>
      val normalizedFeatures = normalizeFeatures(features.toMap, featureMeans.toMap, featureStds.toMap)
      allFeatureNames.map(normalizedFeatures.getOrElse(_, 0.0)).toArray
    }.toArray

    val normalizedTrainTargets = trainData.zip(normalizedTrainAreaValues).zip(normalizedTrainPowerValues).map {
      case (((_, _), normalizedArea), normalizedPower) =>
        Array(normalizedArea, normalizedPower)
    }.toArray

    // Prepare validation data
    val validationAreaValues = validationData.map(_._2._1)
    val validationPowerValues = validationData.map(_._2._2)
    val normalizedValidationAreaValues = normalizeValues(validationAreaValues, areaMean, areaStd)
    val normalizedValidationPowerValues = normalizeValues(validationPowerValues, powerMean, powerStd)

    val normalizedValidationInputs = validationData.map { case (features, _) =>
      val normalizedFeatures = normalizeFeatures(features.toMap, featureMeans.toMap, featureStds.toMap)
      allFeatureNames.map(normalizedFeatures.getOrElse(_, 0.0)).toArray
    }.toArray

    val normalizedValidationTargets = validationData.zip(normalizedValidationAreaValues)
      .zip(normalizedValidationPowerValues).map {
        case (((_, _), normalizedArea), normalizedPower) =>
          Array(normalizedArea, normalizedPower)
      }.toArray

    // Training loop
    var bestValLoss = Double.MaxValue
    var bestNetwork = network
    var patienceCounter = 0

    for (epoch <- 1 to epochs) {
      // Shuffle training data
      val indices = Random.shuffle(normalizedTrainInputs.indices.toList)

      // Mini-batch training
      for (batchStart <- normalizedTrainInputs.indices by batchSize) {
        val batchEndIdx = math.min(batchStart + batchSize, normalizedTrainInputs.length)
        val batchIndices = indices.slice(batchStart, batchEndIdx)

        // Initialize batch gradients with correct dimensions - making sure arrays match the right sizes
        val batchWeightGradients = new Array[Array[Array[Double]]](network.length)
        val batchBiasGradients = new Array[Array[Double]](network.length)

        for (i <- network.indices) {
          val layer = network(i)
          batchWeightGradients(i) = Array.ofDim[Double](layer.weights.length, layer.weights(0).length)
          batchBiasGradients(i) = Array.ofDim[Double](layer.biases.length)
        }

        // Accumulate gradients for batch
        for (idx <- batchIndices) {
          val input = normalizedTrainInputs(idx)
          val target = normalizedTrainTargets(idx)

          val (weightGrads, biasGrads) = backpropagation(input, target, network, l2Lambda)

          // Add to batch gradients
          for (i <- network.indices) {
            for (j <- network(i).weights.indices) {
              for (k <- network(i).weights(0).indices) {
                batchWeightGradients(i)(j)(k) += weightGrads(i)(j)(k)
              }
              batchBiasGradients(i)(j) += biasGrads(i)(j)
            }
          }
        }

        // Average gradients
        val batchCount = batchEndIdx - batchStart
        for (i <- network.indices) {
          for (j <- network(i).weights.indices) {
            for (k <- network(i).weights(0).indices) {
              batchWeightGradients(i)(j)(k) /= batchCount
            }
            batchBiasGradients(i)(j) /= batchCount
          }
        }

        // Update parameters
        network = updateParameters(network, batchWeightGradients, batchBiasGradients, learningRate)
      }

      // Evaluate on training and validation sets
      val trainOutputs = normalizedTrainInputs.map(input => predict(input, network))
      val trainLoss = trainOutputs.zip(normalizedTrainTargets).map { case (pred, target) =>
        mseError(pred, target)
      }.sum / trainOutputs.length

      val valOutputs = normalizedValidationInputs.map(input => predict(input, network))
      val valLoss = valOutputs.zip(normalizedValidationTargets).map { case (pred, target) =>
        mseError(pred, target)
      }.sum / valOutputs.length

      // Separate area and power validation metrics
      val areaValPreds = valOutputs.map(_(0))
      val powerValPreds = valOutputs.map(_(1))

      val areaValTargets = normalizedValidationTargets.map(_(0))
      val powerValTargets = normalizedValidationTargets.map(_(1))

      val areaValMSE = areaValPreds.zip(areaValTargets).map { case (p, t) =>
        math.pow(p - t, 2)
      }.sum / areaValPreds.length

      val powerValMSE = powerValPreds.zip(powerValTargets).map { case (p, t) =>
        math.pow(p - t, 2)
      }.sum / powerValPreds.length

      if (epoch % 10 == 0 || epoch == epochs) {
        // Compute RMSE on the original scale for better interpretability
        val areaValPredsDenormalized = areaValPreds.map(p => p * areaStd + areaMean)
        val powerValPredsDenormalized = powerValPreds.map(p => p * powerStd + powerMean)

        val areaValTargetsDenormalized = validationAreaValues
        val powerValTargetsDenormalized = validationPowerValues

        val areaRMSE = math.sqrt(
          areaValPredsDenormalized.zip(areaValTargetsDenormalized).map { case (p, t) =>
            math.pow(p - t, 2)
          }.sum / areaValPredsDenormalized.length
        )

        val powerRMSE = math.sqrt(
          powerValPredsDenormalized.zip(powerValTargetsDenormalized).map { case (p, t) =>
            math.pow(p - t, 2)
          }.sum / powerValPredsDenormalized.length
        )

        log(f"Epoch $epoch - Train Loss: $trainLoss%.4f, Val Loss: $valLoss%.4f")
        log(f"Area Val RMSE: $areaRMSE%.2f (${areaRMSE/areaMean*100}%.1f%%), " +
          f"Power Val RMSE: $powerRMSE%.2f (${powerRMSE/powerMean*100}%.1f%%)")
      }

      // Early stopping
      if (valLoss < bestValLoss) {
        bestValLoss = valLoss
        bestNetwork = network.map(layer => Layer(
          weights = layer.weights.map(_.clone()),
          biases = layer.biases.clone(),
          activation = layer.activation
        ))
        patienceCounter = 0
      } else {
        patienceCounter += 1
        if (patienceCounter >= patience) {
          log(s"Early stopping at epoch $epoch")
          // Instead of break, return the best model
          return DNNModel(
            layers = bestNetwork,
            featureMeans = featureMeans.toMap,
            featureStds = featureStds.toMap,
            areaMean = areaMean,
            areaStd = areaStd,
            powerMean = powerMean,
            powerStd = powerStd,
            featureNames = allFeatureNames
          )
        }
      }
    }

    // Return the best model with normalization parameters
    DNNModel(
      layers = bestNetwork,
      featureMeans = featureMeans.toMap,
      featureStds = featureStds.toMap,
      areaMean = areaMean,
      areaStd = areaStd,
      powerMean = powerMean,
      powerStd = powerStd,
      featureNames = allFeatureNames
    )
  }

  /**
   * Save model to a file
   */
  def saveModel(model: DNNModel, filePath: String): Try[Unit] = {
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
  def loadModel(filePath: String, loggerOption: LoggerOption = null): Try[DNNModel] = {
    if (loggerOption != null) {
      setMode(loggerOption)
    }

    Try {
      //TODO fix it below
      val file = new File("src/main/resources/" + filePath)
      val inputStream = new FileInputStream(file)
      val objectInputStream = new ObjectInputStream(inputStream)

      try {
        objectInputStream.readObject().asInstanceOf[DNNModel]
      } finally {
        objectInputStream.close()
        inputStream.close()
      }
    }

  }

  /**
   * Predict area and power using the model
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
    model: DNNModel
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
    val featureMap = features.toMap
    val normalizedFeatures = normalizeFeatures(featureMap, model.featureMeans, model.featureStds)

    // Ensure features are in the correct order
    val orderedInputs = model.featureNames.map(normalizedFeatures.getOrElse(_, 0.0)).toArray

    // Make prediction
    val predictions = predict(orderedInputs, model.layers)

    // Denormalize predictions
    val areaPrediction = predictions(0) * model.areaStd + model.areaMean
    val powerPrediction = predictions(1) * model.powerStd + model.powerMean

    // Ensure positive values
    val area = math.max(areaPrediction, 1.0)
    val totalPower = math.max(powerPrediction, 0.001)

    // Divide total power into components (approximate breakdown)
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

  /**
   * Train a new model
   */
  def trainModel(
    weightOutputPath: String,
    trainFilePath: String,
    validationFilePath: String,
    testFilePath: String,
    loggerOption: LoggerOption
  ): Try[DNNModel] = {
    setMode(loggerOption)
    log("Training a new deep neural network model for array synthesis data prediction")

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

      log("Starting neural network training...")
      val model = trainNeuralNetworks(
        trainData = trainData,
        validationData = validationData,
        learningRate = 0.001,
        batchSize = 32,
        epochs = 1000,
        patience = 50,
        l2Lambda = 0.001
      )

      log("Model training completed.")

      evaluateModelOnTestData(model, testData)

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

  private def evaluateModelOnTestData(model: DNNModel, testData: Vector[(FeatureVector, (Double, Double))]): Unit = {
    log("Test Data Evaluation:")

    // Extract test features and normalize
    val testInputs = testData.map { case (features, _) =>
      val normalizedFeatures = normalizeFeatures(features.toMap, model.featureMeans, model.featureStds)
      model.featureNames.map(normalizedFeatures.getOrElse(_, 0.0)).toArray
    }.toArray

    // Get actual area and power values
    val testAreaValues = testData.map(_._2._1)
    val testPowerValues = testData.map(_._2._2)

    // Make predictions
    val testOutputs = testInputs.map(input => predict(input, model.layers))
    val testAreaPreds = testOutputs.map(_(0))
    val testPowerPreds = testOutputs.map(_(1))

    // Denormalize predictions
    val areaPredsDenormalized = testAreaPreds.map(p => p * model.areaStd + model.areaMean)
    val powerPredsDenormalized = testPowerPreds.map(p => p * model.powerStd + model.powerMean)

    // Calculate RMSE for area and power
    val areaRMSE = math.sqrt(
      areaPredsDenormalized.zip(testAreaValues).map { case (p, t) =>
        math.pow(p - t, 2)
      }.sum / areaPredsDenormalized.length
    )

    val powerRMSE = math.sqrt(
      powerPredsDenormalized.zip(testPowerValues).map { case (p, t) =>
        math.pow(p - t, 2)
      }.sum / powerPredsDenormalized.length
    )

    // Calculate Mean Absolute Percentage Error (MAPE)
    val areaMAPE = areaPredsDenormalized.zip(testAreaValues).map { case (p, t) =>
      math.abs((p - t) / t) * 100
    }.sum / areaPredsDenormalized.length

    val powerMAPE = powerPredsDenormalized.zip(testPowerValues).map { case (p, t) =>
      math.abs((p - t) / t) * 100
    }.sum / powerPredsDenormalized.length

    // Calculate R² (coefficient of determination)
    val areaMean = testAreaValues.sum / testAreaValues.length
    val powerMean = testPowerValues.sum / testPowerValues.length

    val areaTSS = testAreaValues.map(t => math.pow(t - areaMean, 2)).sum
    val powerTSS = testPowerValues.map(t => math.pow(t - powerMean, 2)).sum

    val areaRSS = areaPredsDenormalized.zip(testAreaValues).map { case (p, t) =>
      math.pow(t - p, 2)
    }.sum

    val powerRSS = powerPredsDenormalized.zip(testPowerValues).map { case (p, t) =>
      math.pow(t - p, 2)
    }.sum

    val areaR2 = 1 - (areaRSS / areaTSS)
    val powerR2 = 1 - (powerRSS / powerTSS)

    // Log metrics - using s interpolation instead of f interpolation to avoid % issues
    log(f"Area Test RMSE: $areaRMSE%.2f (${areaRMSE/model.areaMean*100}%.1f%%)")
    log(f"Power Test RMSE: $powerRMSE%.2f (${powerRMSE/model.powerMean*100}%.1f%%)")
    log(s"Area Test MAPE: ${areaMAPE.formatted("%.2f")}%")
    log(s"Power Test MAPE: ${powerMAPE.formatted("%.2f")}%")
    log(f"Area Test R²: $areaR2%.4f")
    log(f"Power Test R²: $powerR2%.4f")

    // Print some sample predictions for manual inspection
    log("Sample test predictions (Actual vs Predicted):")
    val numSamples = math.min(5, testAreaValues.length)
    for (i <- 0 until numSamples) {
      log(f"Sample $i: Area (${testAreaValues(i)}%.2f vs ${areaPredsDenormalized(i)}%.2f), " +
        f"Power (${testPowerValues(i)}%.2f vs ${powerPredsDenormalized(i)}%.2f)")
    }

    // Create confusion matrix style analysis for serious mispredictions
    val areaErrorThreshold = 0.15 // 15% error
    val powerErrorThreshold = 0.15 // 15% error

    val severeAreaErrors = areaPredsDenormalized.zip(testAreaValues).count { case (p, t) =>
      math.abs((p - t) / t) > areaErrorThreshold
    }

    val severePowerErrors = powerPredsDenormalized.zip(testPowerValues).count { case (p, t) =>
      math.abs((p - t) / t) > powerErrorThreshold
    }

    log(s"Severe area mispredictions (>15% error): $severeAreaErrors (${(severeAreaErrors.toDouble/testAreaValues.length*100).formatted("%.1f")}%)")
    log(s"Severe power mispredictions (>15% error): $severePowerErrors (${(severePowerErrors.toDouble/testPowerValues.length*100).formatted("%.1f")}%)")

    // If there are configurations where the model performs poorly, log details about those configs
    if (severeAreaErrors > 0 || severePowerErrors > 0) {
      log("Analyzing configurations with poor predictions:")
      val worstAreaPredictions = areaPredsDenormalized.zip(testAreaValues).zip(testData).zipWithIndex
        .map { case (((pred, actual), (features, _)), idx) =>
          (idx, math.abs((pred - actual) / actual), features)
        }
        .sortBy(-_._2)
        .take(3)

      val worstPowerPredictions = powerPredsDenormalized.zip(testPowerValues).zip(testData).zipWithIndex
        .map { case (((pred, actual), (features, _)), idx) =>
          (idx, math.abs((pred - actual) / actual), features)
        }
        .sortBy(-_._2)
        .take(3)

      log("Top 3 worst area predictions:")
      worstAreaPredictions.foreach { case (idx, error, features) =>
        log(s"Index $idx: Error: ${(error*100).formatted("%.1f")}%, " +
          s"Dataflow: ${features.dataflow}, " +
          s"Multipliers: ${features.totalMultipliers}, " +
          s"Config: ${features.r}x${features.c}x${features.a}x${features.b}x${features.p}")
      }

      log("Top 3 worst power predictions:")
      worstPowerPredictions.foreach { case (idx, error, features) =>
        log(s"Index $idx: Error: ${(error*100).formatted("%.1f")}%, " +
          s"Dataflow: ${features.dataflow}, " +
          s"Multipliers: ${features.totalMultipliers}, " +
          s"Config: ${features.r}x${features.c}x${features.a}x${features.b}x${features.p}")
      }
    }
  }


}
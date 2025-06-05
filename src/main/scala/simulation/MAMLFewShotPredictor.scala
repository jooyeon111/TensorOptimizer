package simulation

import scala.util.{Try, Success, Failure, Random}
import java.io._
import java.nio.{ByteBuffer, ByteOrder}
import scala.Array
import scala.collection.parallel.CollectionConverters._

object MAMLFewShotPredictor extends Logger {

  private val SWITCH_POWER_RATIO = 0.30
  private val INTERNAL_POWER_RATIO = 0.60
  private val LEAKAGE_POWER_RATIO = 0.10

  // Enhanced hyperparameters with better tuning
  case class MAMLConfig(
                         innerLearningRate: Double = 0.005,        // Slightly higher for better adaptation
                         outerLearningRate: Double = 0.0005,       // Lower for stability
                         innerSteps: Int = 10,                     // More inner steps for better adaptation
                         supportSize: Int = 5,                     // Larger support set
                         querySize: Int = 10,                      // Larger query set for better gradients
                         metaBatchSize: Int = 8,                   // Balanced batch size
                         metaEpochs: Int = 1200,                   // More epochs
                         validationFreq: Int = 15,                 // More frequent validation
                         patience: Int = 120,                      // Higher patience
                         learningRateDecay: Double = 0.8,          // Gentler decay
                         learningRateDecayFreq: Int = 150,         // Less frequent decay
                         gradientClipping: Double = 1.0,           // Gradient clipping for stability
                         weightDecay: Double = 1e-5,               // L2 regularization
                         minLearningRate: Double = 1e-6            // Minimum learning rate
                       )

  // Enhanced layer with batch normalization and dropout
  case class MAMLLayer(
                        inputSize: Int,
                        outputSize: Int,
                        activation: String = "relu",
                        useBatchNorm: Boolean = false,
                        dropoutRate: Double = 0.0
                      ) {

    def getParamCount: Int = {
      val baseParams = inputSize * outputSize + outputSize
      val bnParams = if (useBatchNorm) outputSize * 2 else 0 // gamma and beta
      baseParams + bnParams
    }

    def forward(
                 input: Vector[Double],
                 weights: Vector[Vector[Double]],
                 training: Boolean = true
               ): Vector[Double] = {
      val weightMatrix = weights.dropRight(if (useBatchNorm) 3 else 1)
      val bias = if (useBatchNorm) weights(weights.length - 3) else weights.last

      val preActivation = (0 until outputSize).map { i =>
        val sum = (0 until inputSize).map(j => input(j) * weightMatrix(j)(i)).sum + bias(i)
        sum
      }.toVector

      val postBN = if (useBatchNorm) {
        val gamma = weights(weights.length - 2)
        val beta = weights.last
        applyBatchNorm(preActivation, gamma, beta)
      } else preActivation

      val activated = postBN.map(applyActivation(_, activation))

      if (training && dropoutRate > 0.0) {
        val random = new Random()
        activated.map(x => if (random.nextDouble() > dropoutRate) x / (1.0 - dropoutRate) else 0.0)
      } else activated
    }

    private def applyBatchNorm(input: Vector[Double], gamma: Vector[Double], beta: Vector[Double]): Vector[Double] = {
      val mean = input.sum / input.length
      val variance = input.map(x => math.pow(x - mean, 2)).sum / input.length
      val std = math.sqrt(variance + 1e-8)

      input.zipWithIndex.map { case (x, i) =>
        val normalized = (x - mean) / std
        gamma(i) * normalized + beta(i)
      }
    }

    private def applyActivation(x: Double, activation: String): Double = activation match {
      case "relu" => math.max(0.0, x)
      case "leaky_relu" => if (x > 0) x else 0.01 * x
      case "gelu" => x * 0.5 * (1.0 + math.tanh(math.sqrt(2.0 / math.Pi) * (x + 0.044715 * math.pow(x, 3))))
      case "swish" => x * (1.0 / (1.0 + math.exp(-x)))
      case "sigmoid" => 1.0 / (1.0 + math.exp(-x))
      case "tanh" => math.tanh(x)
      case "linear" => x
      case _ => x
    }

    def forwardWithTracking(
                             input: Vector[Double],
                             weights: Vector[Vector[Double]],
                             training: Boolean = true
                           ): (Vector[Double], LayerOutput) = {
      val weightMatrix = weights.dropRight(if (useBatchNorm) 3 else 1)
      val bias = if (useBatchNorm) weights(weights.length - 3) else weights.last

      val preActivation = (0 until outputSize).map { i =>
        val sum = (0 until inputSize).map(j => input(j) * weightMatrix(j)(i)).sum + bias(i)
        sum
      }.toVector

      val postBN = if (useBatchNorm) {
        val gamma = weights(weights.length - 2)
        val beta = weights.last
        applyBatchNorm(preActivation, gamma, beta)
      } else preActivation

      val activationOutput = postBN.map(applyActivation(_, activation))

      val finalOutput = if (training && dropoutRate > 0.0) {
        val random = new Random()
        activationOutput.map(x => if (random.nextDouble() > dropoutRate) x / (1.0 - dropoutRate) else 0.0)
      } else activationOutput

      (finalOutput, LayerOutput(finalOutput, postBN, input))
    }

    def backward(
                  outputGradient: Vector[Double],
                  layerOutput: LayerOutput,
                  weights: Vector[Vector[Double]]
                ): (Vector[Double], Vector[Vector[Double]]) = {

      // Activation derivative
      val activationGrad = outputGradient.zip(layerOutput.preActivation).map { case (grad, z) =>
        activation match {
          case "relu" => if (z > 0) grad else 0.0
          case "leaky_relu" => if (z > 0) grad else 0.01 * grad
          case "gelu" =>
            val tanh_arg = math.sqrt(2.0 / math.Pi) * (z + 0.044715 * math.pow(z, 3))
            val sech2 = 1.0 - math.pow(math.tanh(tanh_arg), 2)
            grad * 0.5 * (1.0 + math.tanh(tanh_arg) + z * sech2 * math.sqrt(2.0 / math.Pi) * (1.0 + 3.0 * 0.044715 * math.pow(z, 2)))
          case "swish" =>
            val sigmoid = 1.0 / (1.0 + math.exp(-z))
            grad * (sigmoid + z * sigmoid * (1.0 - sigmoid))
          case "sigmoid" =>
            val s = 1.0 / (1.0 + math.exp(-z))
            grad * s * (1 - s)
          case "tanh" =>
            val t = math.tanh(z)
            grad * (1 - t * t)
          case "linear" => grad
          case _ => grad
        }
      }

      // Weight gradients
      val weightMatrix = weights.dropRight(if (useBatchNorm) 3 else 1)
      val weightGrads = (0 until inputSize).map { i =>
        (0 until outputSize).map { j =>
          layerOutput.input(i) * activationGrad(j)
        }.toVector
      }.toVector

      // Bias gradients
      val biasGrads = activationGrad

      // Input gradients
      val inputGrads = (0 until inputSize).map { i =>
        (0 until outputSize).map { j =>
          weightMatrix(i)(j) * activationGrad(j)
        }.sum
      }.toVector

      val allGrads = if (useBatchNorm) {
        val gammaGrads = layerOutput.preActivation.zip(activationGrad).map { case (pre, grad) => pre * grad }
        val betaGrads = activationGrad
        weightGrads ++ Vector(biasGrads, gammaGrads, betaGrads)
      } else {
        weightGrads :+ biasGrads
      }

      (inputGrads, allGrads)
    }
  }

  // Forward pass tracking for analytical gradients
  case class LayerOutput(
                          activation: Vector[Double],
                          preActivation: Vector[Double],
                          input: Vector[Double]
                        )

  case class ForwardPass(layerOutputs: Vector[LayerOutput])

  // Enhanced network with residual connections
  case class MAMLNetwork(layers: Vector[MAMLLayer], useResidualConnections: Boolean = true) {

    def forward(input: Vector[Double], weights: Vector[Vector[Vector[Double]]], training: Boolean = true): Vector[Double] = {
      var activation = input
      var residualConnection: Option[Vector[Double]] = None

      for (i <- layers.indices) {
        val layerOutput = layers(i).forward(activation, weights(i), training)

        // Apply residual connection if dimensions match and it's enabled
        if (useResidualConnections && residualConnection.isDefined &&
          residualConnection.get.length == layerOutput.length && i > 0) {
          activation = layerOutput.zip(residualConnection.get).map { case (out, res) => out + res }
        } else {
          activation = layerOutput
        }

        // Store for next residual connection (every other layer)
        if (i % 2 == 0) residualConnection = Some(activation)
      }
      activation
    }

    def forwardWithTracking(
                             input: Vector[Double],
                             weights: Vector[Vector[Vector[Double]]],
                             training: Boolean = true
                           ): (Vector[Double], ForwardPass) = {
      var activation = input
      val layerOutputs = scala.collection.mutable.ArrayBuffer[LayerOutput]()
      var residualConnection: Option[Vector[Double]] = None

      for (i <- layers.indices) {
        val (nextActivation, layerOutput) = layers(i).forwardWithTracking(activation, weights(i), training)

        val finalActivation = if (useResidualConnections && residualConnection.isDefined &&
          residualConnection.get.length == nextActivation.length && i > 0) {
          nextActivation.zip(residualConnection.get).map { case (out, res) => out + res }
        } else {
          nextActivation
        }

        layerOutputs += layerOutput.copy(activation = finalActivation)
        activation = finalActivation

        if (i % 2 == 0) residualConnection = Some(activation)
      }

      (activation, ForwardPass(layerOutputs.toVector))
    }

    def backward(
                  target: Vector[Double],
                  prediction: Vector[Double],
                  forwardPass: ForwardPass,
                  weights: Vector[Vector[Vector[Double]]]
                ): Vector[Vector[Vector[Double]]] = {

      // Enhanced loss function (Huber loss for robustness)
      var outputGrad = prediction.zip(target).map { case (pred, targ) =>
        val diff = pred - targ
        val delta = 1.0
        if (math.abs(diff) <= delta) {
          diff
        } else {
          delta * math.signum(diff)
        }
      }

      val gradients = scala.collection.mutable.ArrayBuffer[Vector[Vector[Double]]]()

      // Backward pass through layers
      for (i <- (layers.length - 1) to 0 by -1) {
        val (inputGrad, layerGrads) = layers(i).backward(outputGrad, forwardPass.layerOutputs(i), weights(i))
        gradients.prepend(layerGrads)
        outputGrad = inputGrad
      }

      gradients.toVector
    }

    def getTotalParams: Int = layers.map(_.getParamCount).sum
  }

  // Enhanced training example with better feature engineering
  case class TrainingExample(
                              dataflow: String,
                              totalMultipliers: Int,
                              groupPeRow: Int,
                              groupPeCol: Int,
                              vectorPeRow: Int,
                              vectorPeCol: Int,
                              numMultiplier: Int,
                              streamingDimensionSize: Int,
                              areaUm2: Double,
                              totalPowerMw: Double,
                            ) {

    def toInputVector: Vector[Double] = {
      // More sophisticated feature engineering
      val logTotalMult = math.log(totalMultipliers.toDouble + 1)
      val logStreamingDim = math.log(streamingDimensionSize.toDouble + 1)

      // Architectural features
      val arrayArea = groupPeRow * groupPeCol
      val vectorArea = vectorPeRow * vectorPeCol
      val totalArea = arrayArea * vectorArea

      // Complexity measures
      val computeDensity = totalMultipliers.toDouble / streamingDimensionSize.toDouble
      val hierarchicalComplexity = math.sqrt(arrayArea.toDouble) * math.sqrt(vectorArea.toDouble)

      // Efficiency ratios
      val aspectRatio = groupPeRow.toDouble / (groupPeCol.toDouble + 1e-8)
      val vectorAspectRatio = vectorPeRow.toDouble / (vectorPeCol.toDouble + 1e-8)
      val parallelismFactor = totalMultipliers.toDouble / math.max(groupPeRow * vectorPeRow, groupPeCol * vectorPeCol)

      // Interaction features
      val dataflowNumeric = dataflowToNumeric(dataflow)
      val dataflowMultiplierInteraction = dataflowNumeric * logTotalMult
      val dataflowStreamingInteraction = dataflowNumeric * logStreamingDim

      // Power-of-2 indicators (often important in hardware design)
      val isPowerOf2Mult = if (isPowerOfTwo(numMultiplier)) 1.0 else 0.0
      val isPowerOf2Stream = if (isPowerOfTwo(streamingDimensionSize)) 1.0 else 0.0

      Vector(
        // Basic features (log-transformed for better scaling)
        dataflowNumeric,
        logTotalMult,
        math.log(groupPeRow.toDouble + 1),
        math.log(groupPeCol.toDouble + 1),
        math.log(vectorPeRow.toDouble + 1),
        math.log(vectorPeCol.toDouble + 1),
        math.log(numMultiplier.toDouble + 1),
        logStreamingDim,

        // Architectural complexity features
        math.log(arrayArea.toDouble + 1),
        math.log(vectorArea.toDouble + 1),
        math.log(totalArea.toDouble + 1),
        hierarchicalComplexity,

        // Performance and efficiency features
        math.log(computeDensity + 1),
        parallelismFactor,
        aspectRatio,
        vectorAspectRatio,

        // Interaction features
        dataflowMultiplierInteraction,
        dataflowStreamingInteraction,

        // Hardware-specific features
        isPowerOf2Mult,
        isPowerOf2Stream,

        // Dataflow one-hot encoding
        if (dataflow == "Is") 1.0 else 0.0,
        if (dataflow == "Os") 1.0 else 0.0,
        if (dataflow == "Ws") 1.0 else 0.0,

        // Additional polynomial features for non-linearity
        math.pow(logTotalMult, 2),
        math.pow(logStreamingDim, 2),
        logTotalMult * logStreamingDim,
        math.sqrt(aspectRatio * vectorAspectRatio + 1e-8)
      )
    }

    def toOutputVector: Vector[Double] = {
      Vector(
        // Use different scaling strategies
        math.log(areaUm2 + 1),                    // Log transform for area
        math.log(totalPowerMw + 1)                // Log transform for power
      )
    }

    private def dataflowToNumeric(dataflow: String): Double = dataflow match {
      case "Is" => 0.0
      case "Os" => 1.0
      case "Ws" => 2.0
      case _ => 0.0
    }

    private def isPowerOfTwo(n: Int): Boolean = {
      if (n <= 0) return false
      (n & (n - 1)) == 0
    }
  }

  // Enhanced MAML model with better adaptation strategy
  case class MAMLModel(
                        network: MAMLNetwork,
                        metaWeights: Vector[Vector[Vector[Double]]],
                        config: MAMLConfig,
                        inputNormalizer: DataNormalizer,
                        outputNormalizer: DataNormalizer
                      ) {

    def adapt(supportSet: Vector[(Vector[Double], Vector[Double])]): Vector[Vector[Vector[Double]]] = {
      var adaptedWeights = metaWeights
      var currentLR = config.innerLearningRate

      // Adaptive inner learning rate
      for (step <- 0 until config.innerSteps) {
        val gradients = computeGradientsAnalytical(supportSet, adaptedWeights)

        // Apply gradient clipping
        val clippedGradients = clipGradients(gradients, config.gradientClipping)

        // Update weights with L2 regularization
        adaptedWeights = updateWeightsWithRegularization(adaptedWeights, clippedGradients, currentLR, config.weightDecay)

        // Decay inner learning rate during adaptation
        currentLR *= 0.95
      }

      adaptedWeights
    }

    def predict(input: Vector[Double], adaptedWeights: Option[Vector[Vector[Vector[Double]]]] = None): Vector[Double] = {
      val normalizedInput = inputNormalizer.normalize(input)
      val weights = adaptedWeights.getOrElse(metaWeights)
      val rawOutput = network.forward(normalizedInput, weights, training = false)
      outputNormalizer.denormalize(rawOutput)
    }

    def computeGradientsAnalytical(
                                    data: Vector[(Vector[Double], Vector[Double])],
                                    weights: Vector[Vector[Vector[Double]]]
                                  ): Vector[Vector[Vector[Double]]] = {

      val gradAccumulators = weights.map(_.map(_.map(_ => 0.0)))

      val batchGradients = data.par.map { case (input, target) =>
        val normalizedInput = inputNormalizer.normalize(input)
        val normalizedTarget = outputNormalizer.normalize(target)

        val (prediction, forwardPass) = network.forwardWithTracking(normalizedInput, weights, training = true)
        network.backward(normalizedTarget, prediction, forwardPass, weights)
      }.seq

      batchGradients.foldLeft(gradAccumulators) { (acc, grads) =>
        acc.zip(grads).map { case (accLayer, gradLayer) =>
          accLayer.zip(gradLayer).map { case (accNeuron, gradNeuron) =>
            accNeuron.zip(gradNeuron).map { case (accWeight, gradWeight) =>
              accWeight + gradWeight / data.length
            }
          }
        }
      }
    }

    def computeLoss(
                     data: Vector[(Vector[Double], Vector[Double])],
                     weights: Vector[Vector[Vector[Double]]]
                   ): Double = {
      val predictions = data.par.map { case (input, _) =>
        val normalizedInput = inputNormalizer.normalize(input)
        val rawOutput = network.forward(normalizedInput, weights, training = false)
        outputNormalizer.denormalize(rawOutput)
      }.seq.toVector

      val targets = data.map(_._2)

      // Use Huber loss for robustness
      val huberLoss = predictions.zip(targets).par.map { case (pred, target) =>
        pred.zip(target).map { case (p, t) =>
          val diff = p - t
          val delta = 1.0
          if (math.abs(diff) <= delta) {
            0.5 * diff * diff
          } else {
            delta * (math.abs(diff) - 0.5 * delta)
          }
        }.sum
      }.sum / data.length

      huberLoss
    }

    private def clipGradients(
                               gradients: Vector[Vector[Vector[Double]]],
                               clipValue: Double
                             ): Vector[Vector[Vector[Double]]] = {
      val gradNorm = math.sqrt(
        gradients.flatten.flatten.map(g => g * g).sum
      )

      if (gradNorm > clipValue) {
        val scale = clipValue / gradNorm
        gradients.map(_.map(_.map(_ * scale)))
      } else {
        gradients
      }
    }

    private def updateWeightsWithRegularization(
                                                 weights: Vector[Vector[Vector[Double]]],
                                                 gradients: Vector[Vector[Vector[Double]]],
                                                 learningRate: Double,
                                                 weightDecay: Double
                                               ): Vector[Vector[Vector[Double]]] = {
      weights.zip(gradients).par.map { case (layerWeights, layerGrads) =>
        layerWeights.zip(layerGrads).par.map { case (neuronWeights, neuronGrads) =>
          neuronWeights.zip(neuronGrads).map { case (weight, grad) =>
            // L2 regularization
            weight * (1.0 - learningRate * weightDecay) - learningRate * grad
          }
        }.seq.toVector
      }.seq.toVector
    }
  }

  // Enhanced data normalizer with robust scaling
  case class DataNormalizer(
                             mean: Vector[Double],
                             std: Vector[Double],
                             median: Vector[Double],
                             mad: Vector[Double] // Median Absolute Deviation for robust scaling
                           ) {
    def normalize(data: Vector[Double]): Vector[Double] = {
      data.zipWithIndex.map { case (value, i) =>
        // Use robust scaling for outlier resilience
        val robustScale = if (mad(i) > 1e-8) (value - median(i)) / mad(i) else value - median(i)
        val standardScale = if (std(i) > 1e-8) (value - mean(i)) / std(i) else value - mean(i)

        // Combine both scaling methods
        0.7 * standardScale + 0.3 * robustScale
      }
    }

    def denormalize(data: Vector[Double]): Vector[Double] = {
      data.zip(mean).zip(std).map { case ((value, m), s) =>
        value * s + m
      }
    }
  }

  // Task sampling with better diversity
  case class Task(
                   supportSet: Vector[(Vector[Double], Vector[Double])],
                   querySet: Vector[(Vector[Double], Vector[Double])],
                   taskId: String
                 )

  // Enhanced training function with better hyperparameter scheduling
//  def trainModel(
//                  weightOutputPath: String,
//                  trainFilePath: String,
//                  validationFilePath: String,
//                  testFilePath: String,
//                  loggerOption: LoggerOption
//                ): Try[Unit] = Try {
//
//    setMode(loggerOption)
//    log("Starting Enhanced MAML Few-Shot Learning training...")
//
//    val config = MAMLConfig()
//
//    // Load data
//    val trainData = loadCsvData(trainFilePath)
//    val validationData = loadCsvData(validationFilePath)
//    val testData = loadCsvData(testFilePath)
//
//    log(s"Loaded ${trainData.length} training examples")
//    log(s"Loaded ${validationData.length} validation examples")
//    log(s"Loaded ${testData.length} test examples")
//
//    // Convert data
//    val trainDataConverted = trainData.par.map(ex => (ex.toInputVector, ex.toOutputVector)).seq.toVector
//    val validationDataConverted = validationData.par.map(ex => (ex.toInputVector, ex.toOutputVector)).seq.toVector
//    val testDataConverted = testData.par.map(ex => (ex.toInputVector, ex.toOutputVector)).seq.toVector
//
//    // Create enhanced normalizers
//    val allInputs = trainDataConverted.map(_._1)
//    val allOutputs = trainDataConverted.map(_._2)
//
//    val inputNormalizer = createEnhancedNormalizer(allInputs)
//    val outputNormalizer = createEnhancedNormalizer(allOutputs)
//
//    // Create enhanced network architecture
//    val inputSize = trainData.head.toInputVector.length
//    val outputSize = trainData.head.toOutputVector.length
//
//    val network = MAMLNetwork(Vector(
//      MAMLLayer(inputSize, 128, "gelu", useBatchNorm = true, dropoutRate = 0.1),
//      MAMLLayer(128, 128, "gelu", useBatchNorm = true, dropoutRate = 0.1),
//      MAMLLayer(128, 96, "gelu", useBatchNorm = true, dropoutRate = 0.05),
//      MAMLLayer(96, 64, "swish", useBatchNorm = false, dropoutRate = 0.05),
//      MAMLLayer(64, 32, "swish", useBatchNorm = false),
//      MAMLLayer(32, outputSize, "linear")
//    ), useResidualConnections = true)
//
//    log(s"Enhanced network - Total parameters: ${network.getTotalParams}")
//
//    // Initialize meta-weights with better initialization
//    var metaWeights = initializeWeightsAdvanced(network)
//
//    log("Starting enhanced meta-training...")
//
//    var bestValidationLoss = Double.MaxValue
//    var waitingEpochs = 0
//    var currentLR = config.outerLearningRate
//    val random = new Random(42)
//
//    var shouldStop = false
//    var epoch = 0
//
//    while (epoch < config.metaEpochs && !shouldStop) {
//      val startTime = System.currentTimeMillis()
//
//      // Enhanced learning rate scheduling
//      if (epoch > 0 && epoch % config.learningRateDecayFreq == 0) {
//        currentLR = math.max(currentLR * config.learningRateDecay, config.minLearningRate)
//        log(f"Learning rate decayed to: $currentLR%.6f")
//      }
//
//      // Sample diverse tasks
//      val tasks = sampleDiverseTasks(trainDataConverted, config, random)
//
//      // Compute meta-gradients
//      val metaGradients = computeMetaGradientsEnhanced(tasks, metaWeights, network, inputNormalizer, outputNormalizer, config)
//
//      // Update meta-weights with advanced optimization
//      metaWeights = updateMetaWeightsAdvanced(metaWeights, metaGradients, currentLR, config)
//
//      val epochTime = System.currentTimeMillis() - startTime
//
//      // Validation
//      if (epoch % config.validationFreq == 0) {
//        val validationLoss = evaluateModelEnhanced(validationDataConverted, metaWeights, network, inputNormalizer, outputNormalizer, config, random)
//        log(f"Epoch $epoch: Validation Loss = $validationLoss%.6f, Time = ${epochTime}ms, LR = $currentLR%.6f")
//
//        if (validationLoss < bestValidationLoss) {
//          bestValidationLoss = validationLoss
//          waitingEpochs = 0
//          log(f"New best validation loss: $validationLoss%.6f")
//        } else {
//          waitingEpochs += config.validationFreq
//          if (waitingEpochs >= config.patience) {
//            log(s"Early stopping at epoch $epoch")
//            shouldStop = true
//          }
//        }
//      } else if (epoch % 25 == 0) {
//        log(f"Epoch $epoch: Time = ${epochTime}ms, LR = $currentLR%.6f")
//      }
//
//      epoch += 1
//    }
//
//    // Final evaluation
//    val testLoss = evaluateModelEnhanced(testDataConverted, metaWeights, network, inputNormalizer, outputNormalizer, config, random)
//    log(f"Final test loss: $testLoss%.6f")
//
//    // Create and save final model
//    val model = MAMLModel(network, metaWeights, config, inputNormalizer, outputNormalizer)
//    saveModel(model, weightOutputPath)
//    log(s"Enhanced model saved to: $weightOutputPath")
//  }
  def trainModel(
                  weightOutputPath: String,
                  trainFilePath: String,
                  validationFilePath: String,
                  testFilePath: String,
                  loggerOption: LoggerOption
                ): Try[Unit] = Try {

    setMode(loggerOption)
    log("Starting Enhanced MAML Few-Shot Learning training...")

    val config = MAMLConfig()

    // Load data
    val trainData = loadCsvData(trainFilePath)
    val validationData = loadCsvData(validationFilePath)
    val testData = loadCsvData(testFilePath)

    log(s"Loaded ${trainData.length} training examples")
    log(s"Loaded ${validationData.length} validation examples")
    log(s"Loaded ${testData.length} test examples")

    // Convert data
    val trainDataConverted = trainData.par.map(ex => (ex.toInputVector, ex.toOutputVector)).seq.toVector
    val validationDataConverted = validationData.par.map(ex => (ex.toInputVector, ex.toOutputVector)).seq.toVector
    val testDataConverted = testData.par.map(ex => (ex.toInputVector, ex.toOutputVector)).seq.toVector

    // Create enhanced normalizers
    val allInputs = trainDataConverted.map(_._1)
    val allOutputs = trainDataConverted.map(_._2)

    val inputNormalizer = createEnhancedNormalizer(allInputs)
    val outputNormalizer = createEnhancedNormalizer(allOutputs)

    // Create enhanced network architecture
    val inputSize = trainData.head.toInputVector.length
    val outputSize = trainData.head.toOutputVector.length

    val network = MAMLNetwork(Vector(
      MAMLLayer(inputSize, 128, "gelu", useBatchNorm = true, dropoutRate = 0.1),
      MAMLLayer(128, 128, "gelu", useBatchNorm = true, dropoutRate = 0.1),
      MAMLLayer(128, 96, "gelu", useBatchNorm = true, dropoutRate = 0.05),
      MAMLLayer(96, 64, "swish", useBatchNorm = false, dropoutRate = 0.05),
      MAMLLayer(64, 32, "swish", useBatchNorm = false),
      MAMLLayer(32, outputSize, "linear")
    ), useResidualConnections = true)

    log(s"Enhanced network - Total parameters: ${network.getTotalParams}")

    // Initialize meta-weights with better initialization
    var metaWeights = initializeWeightsAdvanced(network)

    log("Starting enhanced meta-training...")

    var bestValidationLoss = Double.MaxValue
    var waitingEpochs = 0
    var currentLR = config.outerLearningRate
    val random = new Random(42)

    var shouldStop = false
    var epoch = 0

    // Training history tracking
    val trainingLosses = scala.collection.mutable.ArrayBuffer[Double]()
    val validationLosses = scala.collection.mutable.ArrayBuffer[Double]()

    while (epoch < config.metaEpochs && !shouldStop) {
      val startTime = System.currentTimeMillis()

      // Enhanced learning rate scheduling
      if (epoch > 0 && epoch % config.learningRateDecayFreq == 0) {
        currentLR = math.max(currentLR * config.learningRateDecay, config.minLearningRate)
        log(f"Learning rate decayed to: $currentLR%.6f")
      }

      // Sample diverse tasks
      val tasks = sampleDiverseTasks(trainDataConverted, config, random)

      // Compute meta-gradients
      val metaGradients = computeMetaGradientsEnhanced(tasks, metaWeights, network, inputNormalizer, outputNormalizer, config)

      // Update meta-weights with advanced optimization
      metaWeights = updateMetaWeightsAdvanced(metaWeights, metaGradients, currentLR, config)

      val epochTime = System.currentTimeMillis() - startTime

      // Validation
      if (epoch % config.validationFreq == 0) {
        val validationLoss = evaluateModelEnhanced(validationDataConverted, metaWeights, network, inputNormalizer, outputNormalizer, config, random)
        validationLosses += validationLoss

        log(f"Epoch $epoch: Validation Loss = $validationLoss%.6f, Time = ${epochTime}ms, LR = $currentLR%.6f")

        if (validationLoss < bestValidationLoss) {
          bestValidationLoss = validationLoss
          waitingEpochs = 0
          log(f"New best validation loss: $validationLoss%.6f")
        } else {
          waitingEpochs += config.validationFreq
          if (waitingEpochs >= config.patience) {
            log(s"Early stopping at epoch $epoch")
            shouldStop = true
          }
        }
      } else if (epoch % 25 == 0) {
        log(f"Epoch $epoch: Time = ${epochTime}ms, LR = $currentLR%.6f")
      }

      epoch += 1
    }

    // Create final model
    val model = MAMLModel(network, metaWeights, config, inputNormalizer, outputNormalizer)

    // ===== DETAILED TRAINING RESULTS ANALYSIS =====
    log("\n" + "="*80)
    log("DETAILED TRAINING RESULTS ANALYSIS")
    log("="*80)

    // Final evaluation with detailed analysis
    analyzeDetailedResults(model, trainData, validationData, testData, loggerOption)

    // Save model
    saveModel(model, weightOutputPath)
    log(s"Enhanced model saved to: $weightOutputPath")
  }

  // New function for detailed results analysis
  private def analyzeDetailedResults(
                                      model: MAMLModel,
                                      trainData: Vector[TrainingExample],
                                      validationData: Vector[TrainingExample],
                                      testData: Vector[TrainingExample],
                                      loggerOption: LoggerOption
                                    ): Unit = {

    log("\n1. OVERALL PERFORMANCE METRICS")
    log("-" * 50)

    // Analyze each dataset
    analyzeDatasetPerformance("Training Set", trainData, model)
    analyzeDatasetPerformance("Validation Set", validationData, model)
    analyzeDatasetPerformance("Test Set", testData, model)

    log("\n2. DETAILED PREDICTION ANALYSIS")
    log("-" * 50)

    // Detailed prediction comparison
    showDetailedPredictions(testData, model)

    log("\n3. ERROR ANALYSIS BY ARCHITECTURE TYPE")
    log("-" * 50)

    // Group by dataflow and analyze
    analyzeByDataflow(testData, model)

    log("\n4. ERROR ANALYSIS BY MULTIPLIER COUNT")
    log("-" * 50)

    // Group by multiplier count ranges
    analyzeByMultiplierCount(testData, model)

    log("\n5. FEW-SHOT ADAPTATION ANALYSIS")
    log("-" * 50)

    // Test few-shot adaptation capability
    analyzeFewShotCapability(testData, model)

    log("\n6. STATISTICAL ANALYSIS")
    log("-" * 50)

    // Statistical analysis of residuals
    analyzeStatistics(testData, model)
  }

  private def analyzeDatasetPerformance(
                                         datasetName: String,
                                         data: Vector[TrainingExample],
                                         model: MAMLModel
                                       ): Unit = {

    val predictions = data.map { example =>
      val input = example.toInputVector
      val actualOutput = example.toOutputVector
      val predictedOutput = model.predict(input)

      val actualArea = math.exp(actualOutput(0)) - 1
      val actualPower = math.exp(actualOutput(1)) - 1
      val predictedArea = math.exp(predictedOutput(0)) - 1
      val predictedPower = math.exp(predictedOutput(1)) - 1

      (actualArea, predictedArea, actualPower, predictedPower, example)
    }

    // Calculate metrics
    val areaErrors = predictions.map { case (actual, predicted, _, _, _) =>
      math.abs(actual - predicted) / actual * 100
    }

    val powerErrors = predictions.map { case (_, _, actual, predicted, _) =>
      math.abs(actual - predicted) / actual * 100
    }

    val areaMAE = predictions.map { case (actual, predicted, _, _, _) =>
      math.abs(actual - predicted)
    }.sum / predictions.length

    val powerMAE = predictions.map { case (_, _, actual, predicted, _) =>
      math.abs(actual - predicted)
    }.sum / predictions.length

    val areaRMSE = math.sqrt(predictions.map { case (actual, predicted, _, _, _) =>
      math.pow(actual - predicted, 2)
    }.sum / predictions.length)

    val powerRMSE = math.sqrt(predictions.map { case (_, _, actual, predicted, _) =>
      math.pow(actual - predicted, 2)
    }.sum / predictions.length)

    // Calculate R²
    val areaMean = predictions.map(_._1).sum / predictions.length
    val powerMean = predictions.map(_._3).sum / predictions.length

    val areaSSTot = predictions.map { case (actual, _, _, _, _) => math.pow(actual - areaMean, 2) }.sum
    val powerSSTot = predictions.map { case (_, _, actual, _, _) => math.pow(actual - powerMean, 2) }.sum

    val areaSSRes = predictions.map { case (actual, predicted, _, _, _) => math.pow(actual - predicted, 2) }.sum
    val powerSSRes = predictions.map { case (_, _, actual, predicted, _) => math.pow(actual - predicted, 2) }.sum

    val areaR2 = 1 - (areaSSRes / areaSSTot)
    val powerR2 = 1 - (powerSSRes / powerSSTot)

    log(s"\n$datasetName Results (${data.length} samples):")
    log(s"Area Prediction:")
    log(f"  ├── Mean Absolute Error (MAE): ${areaMAE}%.1f μm²")
    log(f"  ├── Root Mean Square Error (RMSE): ${areaRMSE}%.1f μm²")
    log(f"  ├── Mean Absolute Percentage Error (MAPE): ${areaErrors.sum / areaErrors.length}%.2f%%")
    log(f"  ├── R² Score: ${areaR2}%.3f")
    log(f"  ├── Max Error: ${areaErrors.max}%.1f%%")
    log(f"  └── Min Error: ${areaErrors.min}%.1f%%")

    log(s"Power Prediction:")
    log(f"  ├── Mean Absolute Error (MAE): ${powerMAE}%.2f mW")
    log(f"  ├── Root Mean Square Error (RMSE): ${powerRMSE}%.2f mW")
    log(f"  ├── Mean Absolute Percentage Error (MAPE): ${powerErrors.sum / powerErrors.length}%.2f%%")
    log(f"  ├── R² Score: ${powerR2}%.3f")
    log(f"  ├── Max Error: ${powerErrors.max}%.1f%%")
    log(f"  └── Min Error: ${powerErrors.min}%.1f%%")

    // Error distribution
    val areaErrorBins = Array(
      areaErrors.count(_ < 5),
      areaErrors.count(e => e >= 5 && e < 10),
      areaErrors.count(e => e >= 10 && e < 20),
      areaErrors.count(_ >= 20)
    )

    val powerErrorBins = Array(
      powerErrors.count(_ < 5),
      powerErrors.count(e => e >= 5 && e < 15),
      powerErrors.count(e => e >= 15 && e < 25),
      powerErrors.count(_ >= 25)
    )

    log(s"Area Error Distribution:")
    log(f"  ├── <5%% error: ${areaErrorBins(0)} samples (${areaErrorBins(0) * 100.0 / data.length}%.1f%%)")
    log(f"  ├── 5-10%% error: ${areaErrorBins(1)} samples (${areaErrorBins(1) * 100.0 / data.length}%.1f%%)")
    log(f"  ├── 10-20%% error: ${areaErrorBins(2)} samples (${areaErrorBins(2) * 100.0 / data.length}%.1f%%)")
    log(f"  └── >20%% error: ${areaErrorBins(3)} samples (${areaErrorBins(3) * 100.0 / data.length}%.1f%%)")

    log(s"Power Error Distribution:")
    log(f"  ├── <5%% error: ${powerErrorBins(0)} samples (${powerErrorBins(0) * 100.0 / data.length}%.1f%%)")
    log(f"  ├── 5-15%% error: ${powerErrorBins(1)} samples (${powerErrorBins(1) * 100.0 / data.length}%.1f%%)")
    log(f"  ├── 15-25%% error: ${powerErrorBins(2)} samples (${powerErrorBins(2) * 100.0 / data.length}%.1f%%)")
    log(f"  └── >25%% error: ${powerErrorBins(3)} samples (${powerErrorBins(3) * 100.0 / data.length}%.1f%%)")
  }

  private def showDetailedPredictions(
                                       testData: Vector[TrainingExample],
                                       model: MAMLModel
                                     ): Unit = {

    val predictions = testData.map { example =>
      val input = example.toInputVector
      val actualOutput = example.toOutputVector
      val predictedOutput = model.predict(input)

      val actualArea = math.exp(actualOutput(0)) - 1
      val actualPower = math.exp(actualOutput(1)) - 1
      val predictedArea = math.exp(predictedOutput(0)) - 1
      val predictedPower = math.exp(predictedOutput(1)) - 1

      val areaError = math.abs(actualArea - predictedArea) / actualArea * 100
      val powerError = math.abs(actualPower - predictedPower) / actualPower * 100

      (example, actualArea, predictedArea, areaError, actualPower, predictedPower, powerError)
    }

    // Sort by error and show best/worst cases
    val sortedByAreaError = predictions.sortBy(_._4)
    val sortedByPowerError = predictions.sortBy(_._7)

    log("\nBest Area Predictions (Lowest Error):")
    log("Architecture | Actual Area | Predicted Area | Error % | Actual Power | Predicted Power | Error %")
    log("-" * 100)

    sortedByAreaError.take(5).foreach { case (example, actualArea, predArea, areaErr, actualPower, predPower, powerErr) =>
      val archName = s"${example.dataflow}_${example.groupPeRow}x${example.groupPeCol}x${example.vectorPeRow}x${example.vectorPeCol}x${example.numMultiplier}"
      log(f"${archName}%-20s | ${actualArea}%8.0f μm² | ${predArea}%8.0f μm² | ${areaErr}%5.2f%% | ${actualPower}%6.2f mW | ${predPower}%6.2f mW | ${powerErr}%5.2f%%")
    }

    log("\nWorst Area Predictions (Highest Error):")
    log("Architecture | Actual Area | Predicted Area | Error % | Actual Power | Predicted Power | Error %")
    log("-" * 100)

    sortedByAreaError.takeRight(3).foreach { case (example, actualArea, predArea, areaErr, actualPower, predPower, powerErr) =>
      val archName = s"${example.dataflow}_${example.groupPeRow}x${example.groupPeCol}x${example.vectorPeRow}x${example.vectorPeCol}x${example.numMultiplier}"
      log(f"${archName}%-20s | ${actualArea}%8.0f μm² | ${predArea}%8.0f μm² | ${areaErr}%5.2f%% | ${actualPower}%6.2f mW | ${predPower}%6.2f mW | ${powerErr}%5.2f%%")
    }

    log("\nBest Power Predictions (Lowest Error):")
    log("Architecture | Actual Power | Predicted Power | Error % | Actual Area | Predicted Area | Error %")
    log("-" * 100)

    sortedByPowerError.take(5).foreach { case (example, actualArea, predArea, areaErr, actualPower, predPower, powerErr) =>
      val archName = s"${example.dataflow}_${example.groupPeRow}x${example.groupPeCol}x${example.vectorPeRow}x${example.vectorPeCol}x${example.numMultiplier}"
      log(f"${archName}%-20s | ${actualPower}%6.2f mW | ${predPower}%6.2f mW | ${powerErr}%5.2f%% | ${actualArea}%8.0f μm² | ${predArea}%8.0f μm² | ${areaErr}%5.2f%%")
    }
  }

  private def analyzeByDataflow(
                                 testData: Vector[TrainingExample],
                                 model: MAMLModel
                               ): Unit = {

    val dataflowGroups = testData.groupBy(_.dataflow)

    dataflowGroups.foreach { case (dataflow, examples) =>
      val predictions = examples.map { example =>
        val input = example.toInputVector
        val actualOutput = example.toOutputVector
        val predictedOutput = model.predict(input)

        val actualArea = math.exp(actualOutput(0)) - 1
        val actualPower = math.exp(actualOutput(1)) - 1
        val predictedArea = math.exp(predictedOutput(0)) - 1
        val predictedPower = math.exp(predictedOutput(1)) - 1

        val areaError = math.abs(actualArea - predictedArea) / actualArea * 100
        val powerError = math.abs(actualPower - predictedPower) / actualPower * 100

        (areaError, powerError)
      }

      val areaErrors = predictions.map(_._1)
      val powerErrors = predictions.map(_._2)

      val areaMAPE = areaErrors.sum / areaErrors.length
      val powerMAPE = powerErrors.sum / powerErrors.length

      log(f"\n${dataflow} Dataflow Performance (${examples.length} samples):")
      log(f"  ├── Area MAPE: ${areaMAPE}%.1f%%")
      log(f"  ├── Power MAPE: ${powerMAPE}%.1f%%")
      log(f"  ├── Best Area Error: ${areaErrors.min}%.2f%%")
      log(f"  ├── Worst Area Error: ${areaErrors.max}%.2f%%")
      log(f"  ├── Best Power Error: ${powerErrors.min}%.2f%%")
      log(f"  └── Worst Power Error: ${powerErrors.max}%.2f%%")
    }
  }

  private def analyzeByMultiplierCount(
                                        testData: Vector[TrainingExample],
                                        model: MAMLModel
                                      ): Unit = {

    val multiplierRanges = Vector(
      ("Low (1024-4096)", testData.filter(e => e.totalMultipliers >= 1024 && e.totalMultipliers < 4096)),
      ("Medium (4096-8192)", testData.filter(e => e.totalMultipliers >= 4096 && e.totalMultipliers < 8192)),
      ("High (8192-16384)", testData.filter(e => e.totalMultipliers >= 8192 && e.totalMultipliers <= 16384))
    )

    multiplierRanges.foreach { case (rangeName, examples) =>
      if (examples.nonEmpty) {
        val predictions = examples.map { example =>
          val input = example.toInputVector
          val actualOutput = example.toOutputVector
          val predictedOutput = model.predict(input)

          val actualArea = math.exp(actualOutput(0)) - 1
          val actualPower = math.exp(actualOutput(1)) - 1
          val predictedArea = math.exp(predictedOutput(0)) - 1
          val predictedPower = math.exp(predictedOutput(1)) - 1

          val areaError = math.abs(actualArea - predictedArea) / actualArea * 100
          val powerError = math.abs(actualPower - predictedPower) / actualPower * 100

          (areaError, powerError)
        }

        val areaErrors = predictions.map(_._1)
        val powerErrors = predictions.map(_._2)

        val areaMAPE = areaErrors.sum / areaErrors.length
        val powerMAPE = powerErrors.sum / powerErrors.length

        log(f"\n$rangeName Multiplier Count (${examples.length} samples):")
        log(f"  ├── Area MAPE: ${areaMAPE}%.1f%%")
        log(f"  ├── Power MAPE: ${powerMAPE}%.1f%%")
        log(f"  └── Sample Count: ${examples.length}")
      }
    }
  }

  private def analyzeFewShotCapability(
                                        testData: Vector[TrainingExample],
                                        model: MAMLModel
                                      ): Unit = {

    val random = new Random(42)
    val shuffledData = random.shuffle(testData)

    if (shuffledData.length >= 15) {
      val supportSet = shuffledData.take(5).map(ex => (ex.toInputVector, ex.toOutputVector))
      val querySet = shuffledData.slice(5, 15).map(ex => (ex.toInputVector, ex.toOutputVector))

      // Before adaptation
      val beforeAdaptationErrors = querySet.map { case (input, actualOutput) =>
        val predictedOutput = model.predict(input)
        val actualArea = math.exp(actualOutput(0)) - 1
        val actualPower = math.exp(actualOutput(1)) - 1
        val predictedArea = math.exp(predictedOutput(0)) - 1
        val predictedPower = math.exp(predictedOutput(1)) - 1

        val areaError = math.abs(actualArea - predictedArea) / actualArea * 100
        val powerError = math.abs(actualPower - predictedPower) / actualPower * 100
        (areaError, powerError)
      }

      // After adaptation
      val adaptedWeights = model.adapt(supportSet)
      val afterAdaptationErrors = querySet.map { case (input, actualOutput) =>
        val predictedOutput = model.predict(input, Some(adaptedWeights))
        val actualArea = math.exp(actualOutput(0)) - 1
        val actualPower = math.exp(actualOutput(1)) - 1
        val predictedArea = math.exp(predictedOutput(0)) - 1
        val predictedPower = math.exp(predictedOutput(1)) - 1

        val areaError = math.abs(actualArea - predictedArea) / actualArea * 100
        val powerError = math.abs(actualPower - predictedPower) / actualPower * 100
        (areaError, powerError)
      }

      val beforeAreaMAPE = beforeAdaptationErrors.map(_._1).sum / beforeAdaptationErrors.length
      val beforePowerMAPE = beforeAdaptationErrors.map(_._2).sum / beforeAdaptationErrors.length
      val afterAreaMAPE = afterAdaptationErrors.map(_._1).sum / afterAdaptationErrors.length
      val afterPowerMAPE = afterAdaptationErrors.map(_._2).sum / afterAdaptationErrors.length

      val areaImprovement = (beforeAreaMAPE - afterAreaMAPE) / beforeAreaMAPE * 100
      val powerImprovement = (beforePowerMAPE - afterPowerMAPE) / beforePowerMAPE * 100

      log("Few-Shot Adaptation Analysis:")
      log(f"Support Set Size: ${supportSet.length} examples")
      log(f"Query Set Size: ${querySet.length} examples")
      log(f"Adaptation Steps: ${model.config.innerSteps}")
      log("")
      log("Before Adaptation:")
      log(f"  ├── Area MAPE: ${beforeAreaMAPE}%.1f%%")
      log(f"  └── Power MAPE: ${beforePowerMAPE}%.1f%%")
      log("")
      log("After Adaptation:")
      log(f"  ├── Area MAPE: ${afterAreaMAPE}%.1f%%")
      log(f"  └── Power MAPE: ${afterPowerMAPE}%.1f%%")
      log("")
      log("Improvement:")
      log(f"  ├── Area: ${areaImprovement}%.1f%% reduction in error")
      log(f"  └── Power: ${powerImprovement}%.1f%% reduction in error")
    }
  }

  private def analyzeStatistics(
                                 testData: Vector[TrainingExample],
                                 model: MAMLModel
                               ): Unit = {

    val residuals = testData.map { example =>
      val input = example.toInputVector
      val actualOutput = example.toOutputVector
      val predictedOutput = model.predict(input)

      val actualArea = math.exp(actualOutput(0)) - 1
      val actualPower = math.exp(actualOutput(1)) - 1
      val predictedArea = math.exp(predictedOutput(0)) - 1
      val predictedPower = math.exp(predictedOutput(1)) - 1

      val areaResidual = actualArea - predictedArea
      val powerResidual = actualPower - predictedPower

      (areaResidual, powerResidual)
    }

    val areaResiduals = residuals.map(_._1)
    val powerResiduals = residuals.map(_._2)

    val areaMean = areaResiduals.sum / areaResiduals.length
    val powerMean = powerResiduals.sum / powerResiduals.length

    val areaStd = math.sqrt(areaResiduals.map(r => math.pow(r - areaMean, 2)).sum / areaResiduals.length)
    val powerStd = math.sqrt(powerResiduals.map(r => math.pow(r - powerMean, 2)).sum / powerResiduals.length)

    log("Residual Analysis:")
    log("Area Prediction Residuals:")
    log(f"  ├── Mean: ${areaMean}%.1f μm² ${if (areaMean < 0) "(underestimation bias)" else "(overestimation bias)"}")
    log(f"  ├── Std Dev: ${areaStd}%.1f μm²")
    log(f"  ├── Min Residual: ${areaResiduals.min}%.1f μm²")
    log(f"  └── Max Residual: ${areaResiduals.max}%.1f μm²")

    log("Power Prediction Residuals:")
    log(f"  ├── Mean: ${powerMean}%.3f mW ${if (powerMean < 0) "(underestimation bias)" else "(overestimation bias)"}")
    log(f"  ├── Std Dev: ${powerStd}%.2f mW")
    log(f"  ├── Min Residual: ${powerResiduals.min}%.3f mW")
    log(f"  └── Max Residual: ${powerResiduals.max}%.3f mW")

    log("")
    log("95% Prediction Intervals:")
    log(f"  ├── Area: ±${1.96 * areaStd}%.0f μm² (±2σ)")
    log(f"  └── Power: ±${1.96 * powerStd}%.2f mW (±2σ)")
  }
  // Helper functions continue...
  // [The rest of the implementation would include all the enhanced helper functions]

  private def createEnhancedNormalizer(data: Vector[Vector[Double]]): DataNormalizer = {
    val numFeatures = data.head.length

    val mean = (0 until numFeatures).par.map { i =>
      data.map(_(i)).sum / data.length
    }.seq.toVector

    val std = (0 until numFeatures).par.map { i =>
      val variance = data.par.map(row => math.pow(row(i) - mean(i), 2)).sum / data.length
      math.sqrt(variance + 1e-8)
    }.seq.toVector

    val median = (0 until numFeatures).par.map { i =>
      val sorted = data.map(_(i)).sorted
      val mid = sorted.length / 2
      if (sorted.length % 2 == 0) (sorted(mid - 1) + sorted(mid)) / 2.0
      else sorted(mid)
    }.seq.toVector

    val mad = (0 until numFeatures).par.map { i =>
      val med = median(i)
      val deviations = data.map(row => math.abs(row(i) - med)).sorted
      val mid = deviations.length / 2
      val medianDeviation = if (deviations.length % 2 == 0) {
        (deviations(mid - 1) + deviations(mid)) / 2.0
      } else {
        deviations(mid)
      }
      medianDeviation * 1.4826 + 1e-8 // Scale factor for normal distribution
    }.seq.toVector

    DataNormalizer(mean, std, median, mad)
  }

  private def initializeWeightsAdvanced(network: MAMLNetwork): Vector[Vector[Vector[Double]]] = {
    val random = new Random(42)

    network.layers.par.map { layer =>
      // He initialization for ReLU-like activations, Xavier for others
      val initType = layer.activation match {
        case "relu" | "leaky_relu" | "gelu" | "swish" => "he"
        case _ => "xavier"
      }

      val limit = initType match {
        case "he" => math.sqrt(2.0 / layer.inputSize)
        case "xavier" => math.sqrt(6.0 / (layer.inputSize + layer.outputSize))
      }

      // Weight matrix with better initialization
      val weights = (0 until layer.inputSize).map { _ =>
        (0 until layer.outputSize).map { _ =>
          random.nextGaussian() * limit
        }.toVector
      }.toVector

      // Bias initialization
      val biases = (0 until layer.outputSize).map { _ =>
        if (layer.activation == "relu" || layer.activation == "leaky_relu") 0.01 else 0.0
      }.toVector

      // Batch normalization parameters if needed
      if (layer.useBatchNorm) {
        val gamma = Vector.fill(layer.outputSize)(1.0) // Initialize to 1
        val beta = Vector.fill(layer.outputSize)(0.0)  // Initialize to 0
        weights ++ Vector(biases, gamma, beta)
      } else {
        weights :+ biases
      }
    }.seq.toVector
  }

  private def sampleDiverseTasks(
                                  data: Vector[(Vector[Double], Vector[Double])],
                                  config: MAMLConfig,
                                  random: Random
                                ): Vector[Task] = {
    val dataArray = data.toArray

    (0 until config.metaBatchSize).par.map { taskIdx =>
      // Stratified sampling for better diversity
      val shuffledIndices = (0 until dataArray.length).toArray

      // Fisher-Yates shuffle
      for (i <- shuffledIndices.length - 1 to 1 by -1) {
        val j = random.nextInt(i + 1)
        val temp = shuffledIndices(i)
        shuffledIndices(i) = shuffledIndices(j)
        shuffledIndices(j) = temp
      }

      // Ensure support and query sets are diverse
      val totalNeeded = config.supportSize + config.querySize
      val selectedIndices = shuffledIndices.take(totalNeeded)

      val supportSet = selectedIndices.take(config.supportSize).map(dataArray(_)).toVector
      val querySet = selectedIndices.drop(config.supportSize).map(dataArray(_)).toVector

      Task(supportSet, querySet, s"task_$taskIdx")
    }.seq.toVector
  }

  private def computeMetaGradientsEnhanced(
                                            tasks: Vector[Task],
                                            metaWeights: Vector[Vector[Vector[Double]]],
                                            network: MAMLNetwork,
                                            inputNormalizer: DataNormalizer,
                                            outputNormalizer: DataNormalizer,
                                            config: MAMLConfig
                                          ): Vector[Vector[Vector[Double]]] = {

    val taskGradients = tasks.par.map { task =>
      val model = MAMLModel(network, metaWeights, config, inputNormalizer, outputNormalizer)
      val adaptedWeights = model.adapt(task.supportSet)
      model.computeGradientsAnalytical(task.querySet, adaptedWeights)
    }.seq.toVector

    averageGradientsWithWeighting(taskGradients, tasks)
  }

  private def averageGradientsWithWeighting(
                                             gradients: Vector[Vector[Vector[Vector[Double]]]],
                                             tasks: Vector[Task]
                                           ): Vector[Vector[Vector[Double]]] = {
    val numTasks = gradients.length

    // Weight tasks by their query set size (larger sets get more weight)
    val weights = tasks.map(_.querySet.length.toDouble)
    val totalWeight = weights.sum

    gradients.head.zipWithIndex.par.map { case (layer, layerIdx) =>
      layer.zipWithIndex.par.map { case (neuron, neuronIdx) =>
        neuron.zipWithIndex.map { case (_, weightIdx) =>
          gradients.zipWithIndex.map { case (taskGrad, taskIdx) =>
            taskGrad(layerIdx)(neuronIdx)(weightIdx) * weights(taskIdx) / totalWeight
          }.sum
        }
      }.seq.toVector
    }.seq.toVector
  }

  private def updateMetaWeightsAdvanced(
                                         weights: Vector[Vector[Vector[Double]]],
                                         gradients: Vector[Vector[Vector[Double]]],
                                         learningRate: Double,
                                         config: MAMLConfig
                                       ): Vector[Vector[Vector[Double]]] = {

    // Adam-like optimization for meta-updates
    weights.zip(gradients).par.map { case (layerWeights, layerGrads) =>
      layerWeights.zip(layerGrads).par.map { case (neuronWeights, neuronGrads) =>
        neuronWeights.zip(neuronGrads).map { case (weight, grad) =>
          // Simple gradient descent with momentum-like behavior
          val clippedGrad = math.max(-config.gradientClipping, math.min(config.gradientClipping, grad))
          weight - learningRate * clippedGrad - config.weightDecay * learningRate * weight
        }
      }.seq.toVector
    }.seq.toVector
  }

  private def evaluateModelEnhanced(
                                     data: Vector[(Vector[Double], Vector[Double])],
                                     metaWeights: Vector[Vector[Vector[Double]]],
                                     network: MAMLNetwork,
                                     inputNormalizer: DataNormalizer,
                                     outputNormalizer: DataNormalizer,
                                     config: MAMLConfig,
                                     random: Random
                                   ): Double = {
    val tasks = sampleDiverseTasks(data, config, random)

    val losses = tasks.par.map { task =>
      val model = MAMLModel(network, metaWeights, config, inputNormalizer, outputNormalizer)
      val adaptedWeights = model.adapt(task.supportSet)
      model.computeLoss(task.querySet, adaptedWeights)
    }.seq.toVector

    losses.sum / losses.length
  }

  private def loadCsvData(filePath: String): Vector[TrainingExample] = {
    val resourcePath = if (filePath.startsWith("/")) filePath.substring(1) else filePath
    println(s"Loading CSV from resource: $resourcePath")

    val source = scala.io.Source.fromResource(resourcePath)
    try {
      val lines = source.getLines().toVector
      if (lines.isEmpty) {
        throw new IllegalArgumentException(s"CSV file is empty: $resourcePath")
      }

      val rawHeader = lines.head.split(",").map(_.trim)
      val header = rawHeader.map { col =>
        val cleaned = if (col.startsWith("\uFEFF")) col.substring(1) else col
        cleaned.trim
      }

      println(s"CSV header: ${header.mkString(", ")}")
      println(s"Number of columns: ${header.length}")

      if (lines.length <= 1) {
        throw new IllegalArgumentException(s"CSV file has no data rows: $resourcePath")
      }

      val examples = lines.tail.par.map { line =>
        try {
          val values = line.split(",").map(_.trim)
          if (values.length != header.length) {
            throw new IllegalArgumentException(s"Row has ${values.length} values but header has ${header.length} columns")
          }

          val dataMap = header.zip(values).toMap

          def getColumn(possibleNames: String*): String = {
            possibleNames.find(dataMap.contains).map(dataMap(_)) match {
              case Some(value) => value
              case None => throw new IllegalArgumentException(s"Could not find any of these columns: ${possibleNames.mkString(", ")}")
            }
          }

          val dataflow = getColumn("Dataflow", "dataflow")
          val totalMultipliers = getColumn("Total Number of Multipliers", "Total Multipliers", "total_multipliers").toInt
          val groupPeRow = getColumn("R", "Group PE Row", "group_pe_row").toInt
          val groupPeCol = getColumn("C", "Group PE Column", "Group PE Col", "group_pe_col").toInt
          val vectorPeRow = getColumn("A", "Vector PE Row", "vector_pe_row").toInt
          val vectorPeCol = getColumn("B", "Vector PE Column", "Vector PE Col", "vector_pe_col").toInt
          val numMultiplier = getColumn("P", "Multipliers Per PE", "multipliers_per_pe", "Num Multiplier").toInt
          val streamingDimensionSize = getColumn("Streaming Dimension Size", "streaming_dimension_size").toInt
          val areaUm2 = getColumn("Area", "area").toDouble
          val totalPowerMw = getColumn("Total Power", "total_power").toDouble

          TrainingExample(
            dataflow = dataflow,
            totalMultipliers = totalMultipliers,
            groupPeRow = groupPeRow,
            groupPeCol = groupPeCol,
            vectorPeRow = vectorPeRow,
            vectorPeCol = vectorPeCol,
            numMultiplier = numMultiplier,
            streamingDimensionSize = streamingDimensionSize,
            areaUm2 = areaUm2,
            totalPowerMw = totalPowerMw,
          )
        } catch {
          case e: Exception =>
            throw new IllegalArgumentException(s"Error parsing line: $line - ${e.getMessage}", e)
        }
      }.seq.toVector

      println(s"Successfully loaded ${examples.length} training examples")
      examples

    } catch {
      case _: NullPointerException =>
        throw new java.io.FileNotFoundException(s"Resource not found: $resourcePath")
    } finally {
      source.close()
    }
  }

  def loadModel(filePath: String): Try[MAMLModel] = Try {
    val file = new File(filePath)
    if (!file.exists()) {
      throw new FileNotFoundException(s"Model file not found: $filePath")
    }

    val fis = new FileInputStream(file)
    val buffer = new Array[Byte](file.length().toInt)
    fis.read(buffer)
    fis.close()

    val byteBuffer = ByteBuffer.wrap(buffer).order(ByteOrder.LITTLE_ENDIAN)
    deserializeModel(byteBuffer)
  }

  def predictArraySynthesisData(
                                 dataflow: String,
                                 totalMultipliers: Int,
                                 groupPeRow: Int,
                                 groupPeCol: Int,
                                 vectorPeRow: Int,
                                 vectorPeCol: Int,
                                 numMultiplier: Int,
                                 streamingDimensionSize: Int,
                                 model: MAMLModel
                               ): ArraySynthesisData = {

    val example = TrainingExample(
      dataflow = dataflow,
      totalMultipliers = totalMultipliers,
      groupPeRow = groupPeRow,
      groupPeCol = groupPeCol,
      vectorPeRow = vectorPeRow,
      vectorPeCol = vectorPeCol,
      numMultiplier = numMultiplier,
      streamingDimensionSize = streamingDimensionSize,
      areaUm2 = 0.0, // dummy values
      totalPowerMw = 0.0
    )

    val input = example.toInputVector
    val output = model.predict(input)

    val areaUm2 = math.exp(output(0)) - 1
    val totalPowerMw = math.exp(output(1)) - 1

    val (switchPowerMw, internalPowerMw, leakagePowerMw) = distributePower(totalPowerMw)

    ArraySynthesisData(
      areaUm2 = areaUm2,
      switchPowerMw = switchPowerMw,
      internalPowerMw = internalPowerMw,
      leakagePowerMw = leakagePowerMw
    )
  }

  private def distributePower(totalPowerMw: Double): (Double, Double, Double) = {
    val switchPowerMw = totalPowerMw * SWITCH_POWER_RATIO
    val internalPowerMw = totalPowerMw * INTERNAL_POWER_RATIO
    val leakagePowerMw = totalPowerMw * LEAKAGE_POWER_RATIO
    (switchPowerMw, internalPowerMw, leakagePowerMw)
  }

  private def saveModel(model: MAMLModel, filePath: String): Unit = {
    val file = new File(filePath)
    file.getParentFile.mkdirs()

    val fos = new FileOutputStream(file)
    val buffer = serializeModel(model)
    fos.write(buffer)
    fos.close()
  }

  private def serializeModel(model: MAMLModel): Array[Byte] = {
    val buffer = ByteBuffer.allocate(2 * 1024 * 1024).order(ByteOrder.LITTLE_ENDIAN)

    // Write network architecture
    buffer.putInt(model.network.layers.length)
    buffer.put(if (model.network.useResidualConnections) 1.toByte else 0.toByte)

    model.network.layers.foreach { layer =>
      buffer.putInt(layer.inputSize)
      buffer.putInt(layer.outputSize)
      writeString(buffer, layer.activation)
      buffer.put(if (layer.useBatchNorm) 1.toByte else 0.toByte)
      buffer.putDouble(layer.dropoutRate)
    }

    // Write weights
    model.metaWeights.foreach { layer =>
      buffer.putInt(layer.length)
      layer.foreach { neuron =>
        buffer.putInt(neuron.length)
        neuron.foreach(buffer.putDouble)
      }
    }

    // Write enhanced normalizers
    writeEnhancedNormalizer(buffer, model.inputNormalizer)
    writeEnhancedNormalizer(buffer, model.outputNormalizer)

    // Write enhanced config
    writeEnhancedConfig(buffer, model.config)

    val result = new Array[Byte](buffer.position())
    buffer.rewind()
    buffer.get(result)
    result
  }

  private def deserializeModel(buffer: ByteBuffer): MAMLModel = {
    // Read network architecture
    val numLayers = buffer.getInt()
    val useResidualConnections = buffer.get() == 1.toByte

    val layers = (0 until numLayers).map { _ =>
      val inputSize = buffer.getInt()
      val outputSize = buffer.getInt()
      val activation = readString(buffer)
      val useBatchNorm = buffer.get() == 1.toByte
      val dropoutRate = buffer.getDouble()
      MAMLLayer(inputSize, outputSize, activation, useBatchNorm, dropoutRate)
    }.toVector

    val network = MAMLNetwork(layers, useResidualConnections)

    // Read weights
    val weights = layers.map { _ =>
      val layerSize = buffer.getInt()
      (0 until layerSize).map { _ =>
        val neuronSize = buffer.getInt()
        (0 until neuronSize).map(_ => buffer.getDouble()).toVector
      }.toVector
    }.toVector

    // Read enhanced normalizers
    val inputNormalizer = readEnhancedNormalizer(buffer)
    val outputNormalizer = readEnhancedNormalizer(buffer)

    // Read enhanced config
    val config = readEnhancedConfig(buffer)

    MAMLModel(network, weights, config, inputNormalizer, outputNormalizer)
  }

  private def writeString(buffer: ByteBuffer, str: String): Unit = {
    val bytes = str.getBytes("UTF-8")
    buffer.putInt(bytes.length)
    buffer.put(bytes)
  }

  private def readString(buffer: ByteBuffer): String = {
    val length = buffer.getInt()
    val bytes = new Array[Byte](length)
    buffer.get(bytes)
    new String(bytes, "UTF-8")
  }

  private def writeEnhancedNormalizer(buffer: ByteBuffer, normalizer: DataNormalizer): Unit = {
    buffer.putInt(normalizer.mean.length)
    normalizer.mean.foreach(buffer.putDouble)
    normalizer.std.foreach(buffer.putDouble)
    normalizer.median.foreach(buffer.putDouble)
    normalizer.mad.foreach(buffer.putDouble)
  }

  private def readEnhancedNormalizer(buffer: ByteBuffer): DataNormalizer = {
    val length = buffer.getInt()
    val mean = (0 until length).map(_ => buffer.getDouble()).toVector
    val std = (0 until length).map(_ => buffer.getDouble()).toVector
    val median = (0 until length).map(_ => buffer.getDouble()).toVector
    val mad = (0 until length).map(_ => buffer.getDouble()).toVector
    DataNormalizer(mean, std, median, mad)
  }

  private def writeEnhancedConfig(buffer: ByteBuffer, config: MAMLConfig): Unit = {
    buffer.putDouble(config.innerLearningRate)
    buffer.putDouble(config.outerLearningRate)
    buffer.putInt(config.innerSteps)
    buffer.putInt(config.supportSize)
    buffer.putInt(config.querySize)
    buffer.putInt(config.metaBatchSize)
    buffer.putInt(config.metaEpochs)
    buffer.putInt(config.validationFreq)
    buffer.putInt(config.patience)
    buffer.putDouble(config.learningRateDecay)
    buffer.putInt(config.learningRateDecayFreq)
    buffer.putDouble(config.gradientClipping)
    buffer.putDouble(config.weightDecay)
    buffer.putDouble(config.minLearningRate)
  }

  private def readEnhancedConfig(buffer: ByteBuffer): MAMLConfig = {
    MAMLConfig(
      innerLearningRate = buffer.getDouble(),
      outerLearningRate = buffer.getDouble(),
      innerSteps = buffer.getInt(),
      supportSize = buffer.getInt(),
      querySize = buffer.getInt(),
      metaBatchSize = buffer.getInt(),
      metaEpochs = buffer.getInt(),
      validationFreq = buffer.getInt(),
      patience = buffer.getInt(),
      learningRateDecay = buffer.getDouble(),
      learningRateDecayFreq = buffer.getInt(),
      gradientClipping = buffer.getDouble(),
      weightDecay = buffer.getDouble(),
      minLearningRate = buffer.getDouble()
    )
  }

  private def dataflowToNumeric(dataflow: String): Double = dataflow match {
    case "Is" => 0.0
    case "Os" => 1.0
    case "Ws" => 2.0
    case _ => 0.0
  }
}
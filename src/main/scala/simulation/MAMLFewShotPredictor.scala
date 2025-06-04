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

  private def distributePower(totalPowerMw: Double): (Double, Double, Double) = {
    val switchPowerMw = totalPowerMw * SWITCH_POWER_RATIO
    val internalPowerMw = totalPowerMw * INTERNAL_POWER_RATIO
    val leakagePowerMw = totalPowerMw * LEAKAGE_POWER_RATIO
    (switchPowerMw, internalPowerMw, leakagePowerMw)
  }

  // MAML-specific hyperparameters - optimized for speed
  case class MAMLConfig(
                         innerLearningRate: Double = 0.01,
                         outerLearningRate: Double = 0.001,
                         innerSteps: Int = 3,                    // Reduced from 5
                         supportSize: Int = 2,                   // Reduced from 3
                         querySize: Int = 5,                     // Reduced from 7
                         metaBatchSize: Int = 8,                 // Increased for better parallelization
                         metaEpochs: Int = 500,                  // Reduced from 1000
                         validationFreq: Int = 25,               // Reduced from 50
                         patience: Int = 50,                     // Early stopping patience
                         learningRateDecay: Double = 0.5,        // LR decay factor
                         learningRateDecayFreq: Int = 100        // LR decay frequency
                       )

  // Forward pass tracking for analytical gradients
  case class LayerOutput(
                          activation: Vector[Double],
                          preActivation: Vector[Double],
                          input: Vector[Double]
                        )

  case class ForwardPass(layerOutputs: Vector[LayerOutput])

  // Enhanced MAMLLayer with analytical gradients
  case class MAMLLayer(inputSize: Int, outputSize: Int, activation: String = "relu") {

    def getParamCount: Int = inputSize * outputSize + outputSize

    def forward(input: Vector[Double], weights: Vector[Vector[Double]]): Vector[Double] = {
      val weightMatrix = weights.dropRight(1)
      val bias = weights.last

      val output = (0 until outputSize).map { i =>
        val sum = (0 until inputSize).map(j => input(j) * weightMatrix(j)(i)).sum + bias(i)
        activation match {
          case "relu" => math.max(0.0, sum)
          case "sigmoid" => 1.0 / (1.0 + math.exp(-sum))
          case "tanh" => math.tanh(sum)
          case "linear" => sum
          case _ => sum
        }
      }.toVector

      output
    }

    def forwardWithTracking(input: Vector[Double], weights: Vector[Vector[Double]]): (Vector[Double], LayerOutput) = {
      val weightMatrix = weights.dropRight(1)
      val bias = weights.last

      val preActivation = (0 until outputSize).map { i =>
        val sum = (0 until inputSize).map(j => input(j) * weightMatrix(j)(i)).sum + bias(i)
        sum
      }.toVector

      val activationOutput = preActivation.map { z =>
        activation match {
          case "relu" => math.max(0.0, z)
          case "sigmoid" => 1.0 / (1.0 + math.exp(-z))
          case "tanh" => math.tanh(z)
          case "linear" => z
          case _ => z
        }
      }

      (activationOutput, LayerOutput(activationOutput, preActivation, input))
    }

    def backward(
                  outputGradient: Vector[Double],
                  layerOutput: LayerOutput,
                  weights: Vector[Vector[Double]]
                ): (Vector[Double], Vector[Vector[Double]]) = {

      // Compute activation derivative
      val activationGrad = outputGradient.zip(layerOutput.preActivation).map { case (grad, z) =>
        activation match {
          case "relu" => if (z > 0) grad else 0.0
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
      val weightMatrix = weights.dropRight(1)
      val weightGrads = (0 until inputSize).map { i =>
        (0 until outputSize).map { j =>
          layerOutput.input(i) * activationGrad(j)
        }.toVector
      }.toVector

      // Bias gradients
      val biasGrads = activationGrad

      // Input gradients (for backprop to previous layer)
      val inputGrads = (0 until inputSize).map { i =>
        (0 until outputSize).map { j =>
          weightMatrix(i)(j) * activationGrad(j)
        }.sum
      }.toVector

      (inputGrads, weightGrads :+ biasGrads)
    }
  }

  // Enhanced MAMLNetwork with analytical gradients
  case class MAMLNetwork(layers: Vector[MAMLLayer]) {

    def forward(input: Vector[Double], weights: Vector[Vector[Vector[Double]]]): Vector[Double] = {
      var activation = input
      for (i <- layers.indices) {
        activation = layers(i).forward(activation, weights(i))
      }
      activation
    }

    def forwardWithTracking(input: Vector[Double], weights: Vector[Vector[Vector[Double]]]): (Vector[Double], ForwardPass) = {
      var activation = input
      val layerOutputs = scala.collection.mutable.ArrayBuffer[LayerOutput]()

      for (i <- layers.indices) {
        val (nextActivation, layerOutput) = layers(i).forwardWithTracking(activation, weights(i))
        layerOutputs += layerOutput
        activation = nextActivation
      }

      (activation, ForwardPass(layerOutputs.toVector))
    }

    def backward(
                  target: Vector[Double],
                  prediction: Vector[Double],
                  forwardPass: ForwardPass,
                  weights: Vector[Vector[Vector[Double]]]
                ): Vector[Vector[Vector[Double]]] = {

      // MSE loss gradient
      var outputGrad = prediction.zip(target).map { case (pred, targ) => 2.0 * (pred - targ) }

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

  // Task definition for MAML
  case class Task(
                   supportSet: Vector[(Vector[Double], Vector[Double])],
                   querySet: Vector[(Vector[Double], Vector[Double])],
                   taskId: String
                 )

  // MAML model for hardware synthesis prediction
  case class MAMLModel(
                        network: MAMLNetwork,
                        metaWeights: Vector[Vector[Vector[Double]]],
                        config: MAMLConfig,
                        inputNormalizer: DataNormalizer,
                        outputNormalizer: DataNormalizer
                      ) {

    def adapt(supportSet: Vector[(Vector[Double], Vector[Double])]): Vector[Vector[Vector[Double]]] = {
      var adaptedWeights = metaWeights

      // Perform inner loop adaptation
      for (_ <- 0 until config.innerSteps) {
        val gradients = computeGradientsAnalytical(supportSet, adaptedWeights)
        adaptedWeights = updateWeights(adaptedWeights, gradients, config.innerLearningRate)
      }

      adaptedWeights
    }

    def predict(input: Vector[Double], adaptedWeights: Option[Vector[Vector[Vector[Double]]]] = None): Vector[Double] = {
      val normalizedInput = inputNormalizer.normalize(input)
      val weights = adaptedWeights.getOrElse(metaWeights)
      val rawOutput = network.forward(normalizedInput, weights)
      outputNormalizer.denormalize(rawOutput)
    }

    // Fast analytical gradient computation
    def computeGradientsAnalytical(
                                    data: Vector[(Vector[Double], Vector[Double])],
                                    weights: Vector[Vector[Vector[Double]]]
                                  ): Vector[Vector[Vector[Double]]] = {

      // Initialize gradient accumulators
      val gradAccumulators = weights.map(_.map(_.map(_ => 0.0)))

      // Parallelize across data points
      val batchGradients = data.par.map { case (input, target) =>
        val normalizedInput = inputNormalizer.normalize(input)
        val normalizedTarget = outputNormalizer.normalize(target)

        val (prediction, forwardPass) = network.forwardWithTracking(normalizedInput, weights)
        network.backward(normalizedTarget, prediction, forwardPass, weights)
      }.seq

      // Average gradients
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

    // Parallelized loss computation
    def computeLoss(
                     data: Vector[(Vector[Double], Vector[Double])],
                     weights: Vector[Vector[Vector[Double]]]
                   ): Double = {
      // Parallelize prediction computation across data points
      val predictions = data.par.map { case (input, _) =>
        val normalizedInput = inputNormalizer.normalize(input)
        val rawOutput = network.forward(normalizedInput, weights)
        outputNormalizer.denormalize(rawOutput) // Denormalize predictions to original scale
      }.seq.toVector

      val targets = data.map(_._2) // Keep targets in original scale

      // Parallelize MSE computation
      val mse = predictions.zip(targets).par.map { case (pred, target) =>
        pred.zip(target).map { case (p, t) => math.pow(p - t, 2) }.sum
      }.sum / data.length

      mse
    }

    private def updateWeights(
                               weights: Vector[Vector[Vector[Double]]],
                               gradients: Vector[Vector[Vector[Double]]],
                               learningRate: Double
                             ): Vector[Vector[Vector[Double]]] = {
      weights.zip(gradients).par.map { case (layerWeights, layerGrads) =>
        layerWeights.zip(layerGrads).par.map { case (neuronWeights, neuronGrads) =>
          neuronWeights.zip(neuronGrads).map { case (weight, grad) =>
            weight - learningRate * grad
          }
        }.seq.toVector
      }.seq.toVector
    }
  }

  // Data normalizer for input/output scaling
  case class DataNormalizer(
                             mean: Vector[Double],
                             std: Vector[Double]
                           ) {
    def normalize(data: Vector[Double]): Vector[Double] = {
      data.zip(mean).zip(std).map { case ((value, m), s) =>
        if (s > 1e-8) (value - m) / s else value - m
      }
    }

    def denormalize(data: Vector[Double]): Vector[Double] = {
      data.zip(mean).zip(std).map { case ((value, m), s) =>
        value * s + m
      }
    }
  }

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
      Vector(
        dataflowToNumeric(dataflow),
        totalMultipliers.toDouble,
        groupPeRow.toDouble,
        groupPeCol.toDouble,
        vectorPeRow.toDouble,
        vectorPeCol.toDouble,
        numMultiplier.toDouble,
        streamingDimensionSize.toDouble,
        (groupPeRow * groupPeCol).toDouble,
        (vectorPeRow * vectorPeCol).toDouble,
        math.log(totalMultipliers.toDouble + 1),
        math.sqrt(groupPeRow * groupPeCol * vectorPeRow * vectorPeCol)
      )
    }

    def toOutputVector: Vector[Double] = {
      Vector(
        math.log(areaUm2 + 1),
        math.log(totalPowerMw + 1)
      )
    }

    private def dataflowToNumeric(dataflow: String): Double = dataflow match {
      case "Is" => 0.0
      case "Os" => 1.0
      case "Ws" => 2.0
      case _ => 0.0
    }
  }

  def trainModel(
                  weightOutputPath: String,
                  trainFilePath: String,
                  validationFilePath: String,
                  testFilePath: String,
                  loggerOption: LoggerOption
                ): Try[Unit] = Try {

    setMode(loggerOption)
    log("Starting MAML Few-Shot Learning training with analytical gradients...")

    val config = MAMLConfig()

    // Load and parse training data
    val trainData = loadCsvData(trainFilePath)
    val validationData = loadCsvData(validationFilePath)
    val testData = loadCsvData(testFilePath)

    log(s"Loaded ${trainData.length} training examples")
    log(s"Loaded ${validationData.length} validation examples")
    log(s"Loaded ${testData.length} test examples")

    // Pre-convert all data to vectors for efficiency
    val trainDataConverted = trainData.par.map(ex => (ex.toInputVector, ex.toOutputVector)).seq.toVector
    val validationDataConverted = validationData.par.map(ex => (ex.toInputVector, ex.toOutputVector)).seq.toVector
    val testDataConverted = testData.par.map(ex => (ex.toInputVector, ex.toOutputVector)).seq.toVector

    // Create normalizers
    val allInputs = trainDataConverted.map(_._1)
    val allOutputs = trainDataConverted.map(_._2)

    val inputNormalizer = createNormalizer(allInputs)
    val outputNormalizer = createNormalizer(allOutputs)

    // Create network architecture - smaller for speed
    val inputSize = trainData.head.toInputVector.length
    val outputSize = trainData.head.toOutputVector.length

    val network = MAMLNetwork(Vector(
      MAMLLayer(inputSize, 64, "relu"),      // Reduced from 128
      MAMLLayer(64, 32, "relu"),             // Reduced from 64
      MAMLLayer(32, outputSize, "linear")    // Removed one layer
    ))

    log(s"Network architecture: ${inputSize} -> 64 -> 32 -> ${outputSize}")
    log(s"Total parameters: ${network.getTotalParams}")

    // Initialize meta-weights
    var metaWeights = initializeWeights(network)

    log("Starting meta-training with analytical gradients...")

    var bestValidationLoss = Double.MaxValue
    var waitingEpochs = 0
    var currentLR = config.outerLearningRate
    val random = new Random(42)

    // Meta-training loop with early stopping
    var shouldStop = false
    var epoch = 0

    while (epoch < config.metaEpochs && !shouldStop) {
      val startTime = System.currentTimeMillis()

      // Learning rate decay
      if (epoch > 0 && epoch % config.learningRateDecayFreq == 0) {
        currentLR *= config.learningRateDecay
        log(f"Learning rate decayed to: $currentLR%.6f")
      }

      // Sample tasks for this meta-batch
      val tasks = sampleTasksEfficient(trainDataConverted, config, random)

      // Compute meta-gradients using analytical gradients
      val metaGradients = computeMetaGradientsAnalytical(tasks, metaWeights, network, inputNormalizer, outputNormalizer, config)

      // Update meta-weights
      metaWeights = updateMetaWeights(metaWeights, metaGradients, currentLR)

      val epochTime = System.currentTimeMillis() - startTime

      // Validation
      if (epoch % config.validationFreq == 0) {
        val validationLoss = evaluateModelFast(validationDataConverted, metaWeights, network, inputNormalizer, outputNormalizer, config, random)
        log(f"Epoch $epoch: Validation Loss = $validationLoss%.6f, Time = ${epochTime}ms")

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
      } else if (epoch % 10 == 0) {
        log(f"Epoch $epoch: Time = ${epochTime}ms")
      }

      epoch += 1
    }

    // Final evaluation
    val testLoss = evaluateModelFast(testDataConverted, metaWeights, network, inputNormalizer, outputNormalizer, config, random)
    log(f"Final test loss: $testLoss%.6f")

    // Create final model
    val model = MAMLModel(network, metaWeights, config, inputNormalizer, outputNormalizer)

    // Save model
    saveModel(model, weightOutputPath)
    log(s"Model saved to: $weightOutputPath")
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

    val model = deserializeModel(byteBuffer)
    model
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

    val input = Vector(
      dataflowToNumeric(dataflow),
      totalMultipliers.toDouble,
      groupPeRow.toDouble,
      groupPeCol.toDouble,
      vectorPeRow.toDouble,
      vectorPeCol.toDouble,
      numMultiplier.toDouble,
      streamingDimensionSize.toDouble,
      // Add some engineered features
      (groupPeRow * groupPeCol).toDouble,
      (vectorPeRow * vectorPeCol).toDouble,
      math.log(totalMultipliers.toDouble + 1),
      math.sqrt(groupPeRow * groupPeCol * vectorPeRow * vectorPeCol)
    )

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

  // Helper methods
  private def dataflowToNumeric(dataflow: String): Double = dataflow match {
    case "Is" => 0.0
    case "Os" => 1.0
    case "Ws" => 2.0
    case _ => 0.0
  }

  // Efficient task sampling
  private def sampleTasksEfficient(
                                    data: Vector[(Vector[Double], Vector[Double])],
                                    config: MAMLConfig,
                                    random: Random
                                  ): Vector[Task] = {

    // Use array for faster random access
    val dataArray = data.toArray

    (0 until config.metaBatchSize).par.map { taskIdx =>
      // Fisher-Yates shuffle for better performance
      val indices = (0 until dataArray.length).toArray
      for (i <- indices.length - 1 to 1 by -1) {
        val j = random.nextInt(i + 1)
        val temp = indices(i)
        indices(i) = indices(j)
        indices(j) = temp
      }

      val supportSet = indices.take(config.supportSize).map(dataArray(_)).toVector
      val querySet = indices.slice(config.supportSize, config.supportSize + config.querySize).map(dataArray(_)).toVector

      Task(supportSet, querySet, s"task_$taskIdx")
    }.seq.toVector
  }

  // Parallelized CSV data loading
  private def loadCsvData(filePath: String): Vector[TrainingExample] = {
    val resourcePath = if (filePath.startsWith("/")) filePath.substring(1) else filePath

    println(s"Loading CSV from resource: $resourcePath")

    val source = scala.io.Source.fromResource(resourcePath)
    try {
      val lines = source.getLines().toVector
      if (lines.isEmpty) {
        throw new IllegalArgumentException(s"CSV file is empty: $resourcePath")
      }

      // Clean header by removing BOM and extra whitespace
      val rawHeader = lines.head.split(",").map(_.trim)
      val header = rawHeader.map { col =>
        // Remove BOM (UTF-8 BOM is \uFEFF)
        val cleaned = if (col.startsWith("\uFEFF")) col.substring(1) else col
        cleaned.trim
      }

      println(s"Raw CSV header: ${rawHeader.mkString(", ")}")
      println(s"Cleaned CSV header: ${header.mkString(", ")}")
      println(s"Number of columns in header: ${header.length}")

      if (lines.length <= 1) {
        throw new IllegalArgumentException(s"CSV file has no data rows: $resourcePath")
      }

      // Parallelize CSV parsing
      val examples = lines.tail.par.map { line =>
        try {
          val values = line.split(",").map(_.trim)
          if (values.length != header.length) {
            throw new IllegalArgumentException(s"Row has ${values.length} values but header has ${header.length} columns: $line")
          }

          val dataMap = header.zip(values).toMap

          // Use flexible column matching
          def getColumn(possibleNames: String*): String = {
            possibleNames.find(dataMap.contains).map(dataMap(_)) match {
              case Some(value) => value
              case None => throw new IllegalArgumentException(s"Could not find any of these columns: ${possibleNames.mkString(", ")}. Available: ${dataMap.keys.mkString(", ")}")
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
          case e: NumberFormatException =>
            throw new IllegalArgumentException(s"Invalid number format in line: $line", e)
          case e: IllegalArgumentException =>
            throw new IllegalArgumentException(s"Error parsing line: $line - ${e.getMessage}", e)
        }
      }.seq.toVector

      println(s"Successfully loaded ${examples.length} training examples from $resourcePath")
      examples

    } catch {
      case _: NullPointerException =>
        throw new java.io.FileNotFoundException(s"Resource not found: $resourcePath. Make sure the file exists in src/main/resources/$resourcePath")
    } finally {
      source.close()
    }
  }

  // Parallelized normalizer creation
  private def createNormalizer(data: Vector[Vector[Double]]): DataNormalizer = {
    val numFeatures = data.head.length

    // Parallelize mean computation
    val mean = (0 until numFeatures).par.map { i =>
      data.map(_(i)).sum / data.length
    }.seq.toVector

    // Parallelize standard deviation computation
    val std = (0 until numFeatures).par.map { i =>
      val variance = data.par.map(row => math.pow(row(i) - mean(i), 2)).sum / data.length
      math.sqrt(variance + 1e-8)
    }.seq.toVector

    DataNormalizer(mean, std)
  }

  private def initializeWeights(network: MAMLNetwork): Vector[Vector[Vector[Double]]] = {
    val random = new Random(42)

    // Parallelize weight initialization across layers
    network.layers.par.map { layer =>
      // Xavier/Glorot initialization
      val limit = math.sqrt(6.0 / (layer.inputSize + layer.outputSize))

      // Weight matrix - parallelize if layer is large enough
      val weights = if (layer.inputSize * layer.outputSize > 1000) {
        (0 until layer.inputSize).par.map { _ =>
          (0 until layer.outputSize).map { _ =>
            (random.nextDouble() - 0.5) * 2 * limit
          }.toVector
        }.seq.toVector
      } else {
        (0 until layer.inputSize).map { _ =>
          (0 until layer.outputSize).map { _ =>
            (random.nextDouble() - 0.5) * 2 * limit
          }.toVector
        }.toVector
      }

      // Bias vector
      val biases = (0 until layer.outputSize).map { _ =>
        (random.nextDouble() - 0.5) * 0.1
      }.toVector

      weights :+ biases
    }.seq.toVector
  }

  // Parallelized meta-gradient computation with analytical gradients
  private def computeMetaGradientsAnalytical(
                                              tasks: Vector[Task],
                                              metaWeights: Vector[Vector[Vector[Double]]],
                                              network: MAMLNetwork,
                                              inputNormalizer: DataNormalizer,
                                              outputNormalizer: DataNormalizer,
                                              config: MAMLConfig
                                            ): Vector[Vector[Vector[Double]]] = {

    // Parallelize across tasks
    val taskGradients = tasks.par.map { task =>
      // Adapt to task
      val model = MAMLModel(network, metaWeights, config, inputNormalizer, outputNormalizer)
      val adaptedWeights = model.adapt(task.supportSet)

      // Compute gradients on query set using analytical gradients
      model.computeGradientsAnalytical(task.querySet, adaptedWeights)
    }.seq.toVector

    // Average gradients across tasks
    averageGradients(taskGradients)
  }

  // Parallelized gradient averaging
  private def averageGradients(gradients: Vector[Vector[Vector[Vector[Double]]]]): Vector[Vector[Vector[Double]]] = {
    val numTasks = gradients.length

    // Parallelize across layers
    gradients.head.zipWithIndex.par.map { case (layer, layerIdx) =>
      // Parallelize across neurons
      layer.zipWithIndex.par.map { case (neuron, neuronIdx) =>
        neuron.zipWithIndex.map { case (_, weightIdx) =>
          gradients.map(_(layerIdx)(neuronIdx)(weightIdx)).sum / numTasks
        }
      }.seq.toVector
    }.seq.toVector
  }

  // Parallelized meta-weight updates
  private def updateMetaWeights(
                                 weights: Vector[Vector[Vector[Double]]],
                                 gradients: Vector[Vector[Vector[Double]]],
                                 learningRate: Double
                               ): Vector[Vector[Vector[Double]]] = {
    // Parallelize across layers
    weights.zip(gradients).par.map { case (layerWeights, layerGrads) =>
      // Parallelize across neurons
      layerWeights.zip(layerGrads).par.map { case (neuronWeights, neuronGrads) =>
        neuronWeights.zip(neuronGrads).map { case (weight, grad) =>
          weight - learningRate * grad
        }
      }.seq.toVector
    }.seq.toVector
  }

  // Fast model evaluation using analytical gradients
  private def evaluateModelFast(
                                 data: Vector[(Vector[Double], Vector[Double])],
                                 metaWeights: Vector[Vector[Vector[Double]]],
                                 network: MAMLNetwork,
                                 inputNormalizer: DataNormalizer,
                                 outputNormalizer: DataNormalizer,
                                 config: MAMLConfig,
                                 random: Random
                               ): Double = {
    val tasks = sampleTasksEfficient(data, config, random)

    // Parallelize loss computation across tasks
    val losses = tasks.par.map { task =>
      val model = MAMLModel(network, metaWeights, config, inputNormalizer, outputNormalizer)
      val adaptedWeights = model.adapt(task.supportSet)
      model.computeLoss(task.querySet, adaptedWeights)
    }.seq.toVector

    losses.sum / losses.length
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
    val buffer = ByteBuffer.allocate(1024 * 1024).order(ByteOrder.LITTLE_ENDIAN)

    // Write network architecture
    buffer.putInt(model.network.layers.length)
    model.network.layers.foreach { layer =>
      buffer.putInt(layer.inputSize)
      buffer.putInt(layer.outputSize)
      writeString(buffer, layer.activation)
    }

    // Write weights
    model.metaWeights.foreach { layer =>
      buffer.putInt(layer.length)
      layer.foreach { neuron =>
        buffer.putInt(neuron.length)
        neuron.foreach(buffer.putDouble)
      }
    }

    // Write normalizers
    writeNormalizer(buffer, model.inputNormalizer)
    writeNormalizer(buffer, model.outputNormalizer)

    // Write config
    buffer.putDouble(model.config.innerLearningRate)
    buffer.putDouble(model.config.outerLearningRate)
    buffer.putInt(model.config.innerSteps)
    buffer.putInt(model.config.supportSize)
    buffer.putInt(model.config.querySize)
    buffer.putInt(model.config.metaBatchSize)
    buffer.putInt(model.config.metaEpochs)
    buffer.putInt(model.config.validationFreq)
    buffer.putInt(model.config.patience)
    buffer.putDouble(model.config.learningRateDecay)
    buffer.putInt(model.config.learningRateDecayFreq)

    val result = new Array[Byte](buffer.position())
    buffer.rewind()
    buffer.get(result)
    result
  }

  private def deserializeModel(buffer: ByteBuffer): MAMLModel = {
    // Read network architecture
    val numLayers = buffer.getInt()
    val layers = (0 until numLayers).map { _ =>
      val inputSize = buffer.getInt()
      val outputSize = buffer.getInt()
      val activation = readString(buffer)
      MAMLLayer(inputSize, outputSize, activation)
    }.toVector

    val network = MAMLNetwork(layers)

    // Read weights
    val weights = layers.map { _ =>
      val layerSize = buffer.getInt()
      (0 until layerSize).map { _ =>
        val neuronSize = buffer.getInt()
        (0 until neuronSize).map(_ => buffer.getDouble()).toVector
      }.toVector
    }.toVector

    // Read normalizers
    val inputNormalizer = readNormalizer(buffer)
    val outputNormalizer = readNormalizer(buffer)

    // Read config
    val config = MAMLConfig(
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
      learningRateDecayFreq = buffer.getInt()
    )

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

  private def writeNormalizer(buffer: ByteBuffer, normalizer: DataNormalizer): Unit = {
    buffer.putInt(normalizer.mean.length)
    normalizer.mean.foreach(buffer.putDouble)
    normalizer.std.foreach(buffer.putDouble)
  }

  private def readNormalizer(buffer: ByteBuffer): DataNormalizer = {
    val length = buffer.getInt()
    val mean = (0 until length).map(_ => buffer.getDouble()).toVector
    val std = (0 until length).map(_ => buffer.getDouble()).toVector
    DataNormalizer(mean, std)
  }
}
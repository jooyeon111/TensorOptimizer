package simulation

import scala.util.{Try, Success, Failure, Random}
import java.io._
import java.nio.{ByteBuffer, ByteOrder}
import scala.Array

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

  // MAML-specific hyperparameters
  case class MAMLConfig(
    innerLearningRate: Double = 0.01,     // Learning rate for task-specific adaptation
    outerLearningRate: Double = 0.001,    // Meta-learning rate
    innerSteps: Int = 5,                  // Number of gradient steps for task adaptation
    supportSize: Int = 3,                 // Number of examples in support set
    querySize: Int = 7,                  // Number of examples in query set
    metaBatchSize: Int = 4,               // Number of tasks per meta-batch
    metaEpochs: Int = 1000,               // Number of meta-training epochs
    validationFreq: Int = 50              // Validation frequency
  )

  // Neural network architecture for MAML
  case class MAMLNetwork(layers: Vector[MAMLLayer]) {
    def forward(input: Vector[Double], weights: Vector[Vector[Vector[Double]]]): Vector[Double] = {
      var activation = input
      for (i <- layers.indices) {
        activation = layers(i).forward(activation, weights(i))
      }
      activation
    }

    def getTotalParams: Int = layers.map(_.getParamCount).sum
  }

  case class MAMLLayer(inputSize: Int, outputSize: Int, activation: String = "relu") {

    def getParamCount: Int = inputSize * outputSize + outputSize // weights + biases

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
  }

  // Task definition for MAML
  case class Task(
    supportSet: Vector[(Vector[Double], Vector[Double])],
    querySet: Vector[(Vector[Double], Vector[Double])],
    taskId: String
  )

  // MAML model for hardware synthesis prediction

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
        val gradients = computeGradients(supportSet, adaptedWeights)
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

    // Make computeGradients public so it can be called from computeMetaGradients
    def computeGradients(
      data: Vector[(Vector[Double], Vector[Double])],
      weights: Vector[Vector[Vector[Double]]]
    ): Vector[Vector[Vector[Double]]] = {
      val epsilon = 1e-7

      weights.zipWithIndex.map { case (layerWeights, layerIdx) =>
        layerWeights.zipWithIndex.map { case (neuronWeights, neuronIdx) =>
          neuronWeights.zipWithIndex.map { case (weight, weightIdx) =>
            // Numerical gradient computation
            val weightsPlus = updateSingleWeight(weights, layerIdx, neuronIdx, weightIdx, epsilon)
            val weightsMinus = updateSingleWeight(weights, layerIdx, neuronIdx, weightIdx, -epsilon)

            val lossPlus = computeLoss(data, weightsPlus)
            val lossMinus = computeLoss(data, weightsMinus)

            (lossPlus - lossMinus) / (2 * epsilon)
          }
        }
      }
    }

    // Make computeLoss public as well since it's used by computeGradients
    def computeLoss(
      data: Vector[(Vector[Double], Vector[Double])],
      weights: Vector[Vector[Vector[Double]]]
    ): Double = {
      val predictions = data.map { case (input, _) =>
        val normalizedInput = inputNormalizer.normalize(input)
        val rawOutput = network.forward(normalizedInput, weights)
        outputNormalizer.denormalize(rawOutput) // Denormalize predictions to original scale
      }

      val targets = data.map(_._2) // Keep targets in original scale

      // Mean squared error between denormalized predictions and original targets
      val mse = predictions.zip(targets).map { case (pred, target) =>
        pred.zip(target).map { case (p, t) => math.pow(p - t, 2) }.sum
      }.sum / data.length

      mse
    }

    private def updateSingleWeight(
      weights: Vector[Vector[Vector[Double]]],
      layerIdx: Int,
      neuronIdx: Int,
      weightIdx: Int,
      delta: Double
    ): Vector[Vector[Vector[Double]]] = {
      weights.zipWithIndex.map { case (layer, lIdx) =>
        if (lIdx == layerIdx) {
          layer.zipWithIndex.map { case (neuron, nIdx) =>
            if (nIdx == neuronIdx) {
              neuron.zipWithIndex.map { case (w, wIdx) =>
                if (wIdx == weightIdx) w + delta else w
              }
            } else neuron
          }
        } else layer
      }
    }

    private def updateWeights(
      weights: Vector[Vector[Vector[Double]]],
      gradients: Vector[Vector[Vector[Double]]],
      learningRate: Double
    ): Vector[Vector[Vector[Double]]] = {
      weights.zip(gradients).map { case (layerWeights, layerGrads) =>
        layerWeights.zip(layerGrads).map { case (neuronWeights, neuronGrads) =>
          neuronWeights.zip(neuronGrads).map { case (weight, grad) =>
            weight - learningRate * grad
          }
        }
      }
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
    log("Starting MAML Few-Shot Learning training...")

    val config = MAMLConfig()

    // Load and parse training data
    val trainData = loadCsvData(trainFilePath)
    val validationData = loadCsvData(validationFilePath)
    val testData = loadCsvData(testFilePath)

    log(s"Loaded ${trainData.length} training examples")
    log(s"Loaded ${validationData.length} validation examples")
    log(s"Loaded ${testData.length} test examples")

    // Create data normalizers
    val allInputs = trainData.map(_.toInputVector)
    val allOutputs = trainData.map(_.toOutputVector)

    val inputNormalizer = createNormalizer(allInputs)
    val outputNormalizer = createNormalizer(allOutputs)

    // Create network architecture
    val inputSize = trainData.head.toInputVector.length
    val outputSize = trainData.head.toOutputVector.length

    val network = MAMLNetwork(Vector(
      MAMLLayer(inputSize, 128, "relu"),
      MAMLLayer(128, 64, "relu"),
      MAMLLayer(64, 32, "relu"),
      MAMLLayer(32, outputSize, "linear")
    ))

    // Initialize meta-weights
    var metaWeights = initializeWeights(network)

    log("Starting meta-training...")

    var bestValidationLoss = Double.MaxValue
    val random = new Random(42)

    // Meta-training loop
    for (epoch <- 0 until config.metaEpochs) {
      // Sample tasks for this meta-batch
      val tasks = sampleTasks(trainData, config, random)

      // Compute meta-gradients
      val metaGradients = computeMetaGradients(tasks, metaWeights, network, inputNormalizer, outputNormalizer, config)

      // Update meta-weights
      metaWeights = updateMetaWeights(metaWeights, metaGradients, config.outerLearningRate)

      // Validation
      if (epoch % config.validationFreq == 0) {
        val validationLoss = evaluateModel(validationData, metaWeights, network, inputNormalizer, outputNormalizer, config)
        log(f"Epoch $epoch: Validation Loss = $validationLoss%.6f")

        if (validationLoss < bestValidationLoss) {
          bestValidationLoss = validationLoss
          log(f"New best validation loss: $validationLoss%.6f")
        }
      }
    }

    // Final evaluation
    val testLoss = evaluateModel(testData, metaWeights, network, inputNormalizer, outputNormalizer, config)
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

      val examples = lines.tail.map { line =>
        try {
          val values = line.split(",").map(_.trim)
          if (values.length != header.length) {
            throw new IllegalArgumentException(s"Row has ${values.length} values but header has ${header.length} columns: $line")
          }

          val dataMap = header.zip(values).toMap

          // Debug: print the dataMap keys for the first row to see what we have
          if (line == lines.tail.head) {
            println(s"Available columns: ${dataMap.keys.mkString(", ")}")
          }

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

          // Since we only have total power, we need to estimate the breakdown
          // Based on typical hardware, roughly: switch ~30%, internal ~60%, leakage ~10%
          val switchPowerMw = totalPowerMw * 0.3
          val internalPowerMw = totalPowerMw * 0.6
          val leakagePowerMw = totalPowerMw * 0.1

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
//            switchPowerMw = switchPowerMw,
//            internalPowerMw = internalPowerMw,
//            leakagePowerMw = leakagePowerMw
          )
        } catch {
          case e: NumberFormatException =>
            throw new IllegalArgumentException(s"Invalid number format in line: $line", e)
          case e: IllegalArgumentException =>
            throw new IllegalArgumentException(s"Error parsing line: $line - ${e.getMessage}", e)
        }
      }

      println(s"Successfully loaded ${examples.length} training examples from $resourcePath")
      examples

    } catch {
      case _: NullPointerException =>
        throw new java.io.FileNotFoundException(s"Resource not found: $resourcePath. Make sure the file exists in src/main/resources/$resourcePath")
    } finally {
      source.close()
    }
  }


  private def createNormalizer(data: Vector[Vector[Double]]): DataNormalizer = {
    val numFeatures = data.head.length
    val mean = (0 until numFeatures).map { i =>
      data.map(_(i)).sum / data.length
    }.toVector

    val std = (0 until numFeatures).map { i =>
      val variance = data.map(row => math.pow(row(i) - mean(i), 2)).sum / data.length
      math.sqrt(variance + 1e-8)
    }.toVector

    DataNormalizer(mean, std)
  }

  private def initializeWeights(network: MAMLNetwork): Vector[Vector[Vector[Double]]] = {
    val random = new Random(42)

    network.layers.map { layer =>
      // Xavier/Glorot initialization
      val limit = math.sqrt(6.0 / (layer.inputSize + layer.outputSize))

      // Weight matrix
      val weights = (0 until layer.inputSize).map { _ =>
        (0 until layer.outputSize).map { _ =>
          (random.nextDouble() - 0.5) * 2 * limit
        }.toVector
      }.toVector

      // Bias vector
      val biases = (0 until layer.outputSize).map { _ =>
        (random.nextDouble() - 0.5) * 0.1
      }.toVector

      weights :+ biases
    }.toVector
  }

  private def sampleTasks(data: Vector[TrainingExample], config: MAMLConfig, random: Random): Vector[Task] = {
    (0 until config.metaBatchSize).map { taskIdx =>
      val shuffled = random.shuffle(data)
      val supportSet = shuffled.take(config.supportSize).map(ex => (ex.toInputVector, ex.toOutputVector))
      val querySet = shuffled.slice(config.supportSize, config.supportSize + config.querySize).map(ex => (ex.toInputVector, ex.toOutputVector))

      Task(supportSet, querySet, s"task_$taskIdx")
    }.toVector
  }

  private def computeMetaGradients(
    tasks: Vector[Task],
    metaWeights: Vector[Vector[Vector[Double]]],
    network: MAMLNetwork,
    inputNormalizer: DataNormalizer,
    outputNormalizer: DataNormalizer,
    config: MAMLConfig
  ): Vector[Vector[Vector[Double]]] = {

    val taskGradients = tasks.map { task =>
      // Adapt to task
      val model = MAMLModel(network, metaWeights, config, inputNormalizer, outputNormalizer)
      val adaptedWeights = model.adapt(task.supportSet)

      // Compute gradients on query set
      model.computeGradients(task.querySet, adaptedWeights)
    }

    // Average gradients across tasks
    averageGradients(taskGradients)
  }

  private def averageGradients(gradients: Vector[Vector[Vector[Vector[Double]]]]): Vector[Vector[Vector[Double]]] = {
    val numTasks = gradients.length
    gradients.head.zipWithIndex.map { case (layer, layerIdx) =>
      layer.zipWithIndex.map { case (neuron, neuronIdx) =>
        neuron.zipWithIndex.map { case (_, weightIdx) =>
          gradients.map(_(layerIdx)(neuronIdx)(weightIdx)).sum / numTasks
        }
      }
    }
  }

  private def updateMetaWeights(
    weights: Vector[Vector[Vector[Double]]],
    gradients: Vector[Vector[Vector[Double]]],
    learningRate: Double
  ): Vector[Vector[Vector[Double]]] = {
    weights.zip(gradients).map { case (layerWeights, layerGrads) =>
      layerWeights.zip(layerGrads).map { case (neuronWeights, neuronGrads) =>
        neuronWeights.zip(neuronGrads).map { case (weight, grad) =>
          weight - learningRate * grad
        }
      }
    }
  }

  private def evaluateModel(
    data: Vector[TrainingExample],
    metaWeights: Vector[Vector[Vector[Double]]],
    network: MAMLNetwork,
    inputNormalizer: DataNormalizer,
    outputNormalizer: DataNormalizer,
    config: MAMLConfig
  ): Double = {
    val random = new Random(42)
    val tasks = sampleTasks(data, config, random)

    val losses = tasks.map { task =>
      val model = MAMLModel(network, metaWeights, config, inputNormalizer, outputNormalizer)
      val adaptedWeights = model.adapt(task.supportSet)
      model.computeLoss(task.querySet, adaptedWeights)
    }

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
      querySize = buffer.getInt()
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
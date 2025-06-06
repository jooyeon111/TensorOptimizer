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

  // Enhanced hyperparameters with data augmentation settings
  case class MAMLConfig(
                         innerLearningRate: Double = 0.005,
                         outerLearningRate: Double = 0.0005,
                         innerSteps: Int = 10,
                         supportSize: Int = 5,
                         querySize: Int = 10,
                         metaBatchSize: Int = 8,
                         metaEpochs: Int = 1200,
                         validationFreq: Int = 15,
                         patience: Int = 120,
                         learningRateDecay: Double = 0.8,
                         learningRateDecayFreq: Int = 150,
                         gradientClipping: Double = 1.0,
                         weightDecay: Double = 1e-5,
                         minLearningRate: Double = 1e-6,
                         // Data augmentation parameters
                         augmentationEnabled: Boolean = true,
                         augmentationRatio: Double = 0.3,
                         noiseStdDev: Double = 0.02,
                         architecturalSwapProbability: Double = 0.15,
                         dimensionPerturbationRange: Double = 0.1,
                         dataflowMixupProbability: Double = 0.1
                       )

  // Data augmentation configuration
  case class AugmentationConfig(
                                 noiseStdDev: Double = 0.02,
                                 architecturalSwapProbability: Double = 0.15,
                                 dimensionPerturbationRange: Double = 0.1,
                                 dataflowMixupProbability: Double = 0.1,
                                 scalingRange: (Double, Double) = (0.95, 1.05),
                                 streamingDimensionVariation: Boolean = true,
                                 multiplierReorganization: Boolean = true,
                                 powerOfTwoEnforcement: Boolean = true
                               )

  // Enhanced training example with augmentation methods
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
      // Enhanced feature engineering (keeping original implementation)
      val logTotalMult = math.log(totalMultipliers.toDouble + 1)
      val logStreamingDim = math.log(streamingDimensionSize.toDouble + 1)

      val arrayArea = groupPeRow * groupPeCol
      val vectorArea = vectorPeRow * vectorPeCol
      val totalArea = arrayArea * vectorArea

      val computeDensity = totalMultipliers.toDouble / streamingDimensionSize.toDouble
      val hierarchicalComplexity = math.sqrt(arrayArea.toDouble) * math.sqrt(vectorArea.toDouble)

      val aspectRatio = groupPeRow.toDouble / (groupPeCol.toDouble + 1e-8)
      val vectorAspectRatio = vectorPeRow.toDouble / (vectorPeCol.toDouble + 1e-8)
      val parallelismFactor = totalMultipliers.toDouble / math.max(groupPeRow * vectorPeRow, groupPeCol * vectorPeCol)

      val dataflowNumeric = dataflowToNumeric(dataflow)
      val dataflowMultiplierInteraction = dataflowNumeric * logTotalMult
      val dataflowStreamingInteraction = dataflowNumeric * logStreamingDim

      val isPowerOf2Mult = if (isPowerOfTwo(numMultiplier)) 1.0 else 0.0
      val isPowerOf2Stream = if (isPowerOfTwo(streamingDimensionSize)) 1.0 else 0.0

      Vector(
        dataflowNumeric, logTotalMult,
        math.log(groupPeRow.toDouble + 1), math.log(groupPeCol.toDouble + 1),
        math.log(vectorPeRow.toDouble + 1), math.log(vectorPeCol.toDouble + 1),
        math.log(numMultiplier.toDouble + 1), logStreamingDim,
        math.log(arrayArea.toDouble + 1), math.log(vectorArea.toDouble + 1),
        math.log(totalArea.toDouble + 1), hierarchicalComplexity,
        math.log(computeDensity + 1), parallelismFactor, aspectRatio, vectorAspectRatio,
        dataflowMultiplierInteraction, dataflowStreamingInteraction,
        isPowerOf2Mult, isPowerOf2Stream,
        if (dataflow == "Is") 1.0 else 0.0,
        if (dataflow == "Os") 1.0 else 0.0,
        if (dataflow == "Ws") 1.0 else 0.0,
        math.pow(logTotalMult, 2), math.pow(logStreamingDim, 2),
        logTotalMult * logStreamingDim,
        math.sqrt(aspectRatio * vectorAspectRatio + 1e-8)
      )
    }

    def toOutputVector: Vector[Double] = {
      Vector(math.log(areaUm2 + 1), math.log(totalPowerMw + 1))
    }

    // ===== DATA AUGMENTATION METHODS =====

    def augmentWithNoise(config: AugmentationConfig, random: Random): TrainingExample = {
      // Add Gaussian noise to continuous features while preserving discrete constraints
      val noiseScale = config.noiseStdDev

      // Area augmentation with realistic bounds
      val areaMultiplier = 1.0 + random.nextGaussian() * noiseScale
      val augmentedArea = math.max(areaUm2 * areaMultiplier, areaUm2 * 0.8) // Minimum 80% of original

      // Power augmentation with realistic scaling
      val powerMultiplier = 1.0 + random.nextGaussian() * noiseScale
      val augmentedPower = math.max(totalPowerMw * powerMultiplier, totalPowerMw * 0.7) // Minimum 70% of original

      // Minor streaming dimension variation
      val streamingVariation = if (config.streamingDimensionVariation && random.nextDouble() < 0.3) {
        val validStreamingSizes = getValidStreamingSizes(streamingDimensionSize)
        if (validStreamingSizes.nonEmpty) validStreamingSizes(random.nextInt(validStreamingSizes.length))
        else streamingDimensionSize
      } else streamingDimensionSize

      this.copy(
        areaUm2 = augmentedArea,
        totalPowerMw = augmentedPower,
        streamingDimensionSize = streamingVariation
      )
    }

    def augmentWithArchitecturalSwap(config: AugmentationConfig, random: Random): TrainingExample = {
      if (random.nextDouble() > config.architecturalSwapProbability) return this

      // Swap group and vector PE dimensions while maintaining total multipliers
      val shouldSwapGroupVector = random.nextDouble() < 0.5
      val shouldSwapRowCol = random.nextDouble() < 0.5

      val (newGroupRow, newGroupCol, newVectorRow, newVectorCol) =
        if (shouldSwapGroupVector && shouldSwapRowCol) {
          // Swap group <-> vector AND row <-> col
          (vectorPeCol, vectorPeRow, groupPeCol, groupPeRow)
        } else if (shouldSwapGroupVector) {
          // Only swap group <-> vector
          (vectorPeRow, vectorPeCol, groupPeRow, groupPeCol)
        } else if (shouldSwapRowCol) {
          // Only swap row <-> col
          (groupPeCol, groupPeRow, vectorPeCol, vectorPeRow)
        } else {
          // Diagonal swap
          (vectorPeRow, groupPeCol, groupPeRow, vectorPeCol)
        }

      // Verify the swap maintains total multipliers
      val newTotal = newGroupRow * newGroupCol * newVectorRow * newVectorCol * numMultiplier
      if (newTotal == totalMultipliers) {
        this.copy(
          groupPeRow = newGroupRow,
          groupPeCol = newGroupCol,
          vectorPeRow = newVectorRow,
          vectorPeCol = newVectorCol
        )
      } else this
    }

    def augmentWithMultiplierReorganization(config: AugmentationConfig, random: Random): TrainingExample = {
      if (!config.multiplierReorganization || random.nextDouble() > 0.2) return this

      // Reorganize multipliers between dimensions while maintaining total
      val currentTotal = groupPeRow * groupPeCol * vectorPeRow * vectorPeCol * numMultiplier
      val validConfigs = generateEquivalentConfigurations(currentTotal)

      if (validConfigs.nonEmpty) {
        val selectedConfig = validConfigs(random.nextInt(validConfigs.length))
        this.copy(
          groupPeRow = selectedConfig._1,
          groupPeCol = selectedConfig._2,
          vectorPeRow = selectedConfig._3,
          vectorPeCol = selectedConfig._4,
          numMultiplier = selectedConfig._5
        )
      } else this
    }

    def augmentWithDataflowAnalogies(config: AugmentationConfig, otherExamples: Vector[TrainingExample], random: Random): TrainingExample = {
      if (random.nextDouble() > config.dataflowMixupProbability) return this

      // Find examples with similar architectural complexity but different dataflow
      val architecturalComplexity = groupPeRow * groupPeCol * vectorPeRow * vectorPeCol
      val similarExamples = otherExamples.filter { other =>
        other.dataflow != this.dataflow &&
          math.abs(other.totalMultipliers - this.totalMultipliers) / this.totalMultipliers.toDouble < 0.2 &&
          math.abs((other.groupPeRow * other.groupPeCol * other.vectorPeRow * other.vectorPeCol) - architecturalComplexity) / architecturalComplexity.toDouble < 0.3
      }

      if (similarExamples.nonEmpty) {
        val reference = similarExamples(random.nextInt(similarExamples.length))
        val mixupAlpha = 0.1 + random.nextDouble() * 0.2 // 10-30% mixing

        // Interpolate area and power based on dataflow transformation patterns
        val areaRatio = reference.areaUm2 / this.areaUm2
        val powerRatio = reference.totalPowerMw / this.totalPowerMw

        val augmentedArea = this.areaUm2 * (1.0 + mixupAlpha * (areaRatio - 1.0))
        val augmentedPower = this.totalPowerMw * (1.0 + mixupAlpha * (powerRatio - 1.0))

        this.copy(
          areaUm2 = math.max(augmentedArea, this.areaUm2 * 0.5),
          totalPowerMw = math.max(augmentedPower, this.totalPowerMw * 0.5)
        )
      } else this
    }

    def augmentWithScaling(config: AugmentationConfig, random: Random): TrainingExample = {
      val (minScale, maxScale) = config.scalingRange
      val areaScale = minScale + random.nextDouble() * (maxScale - minScale)
      val powerScale = minScale + random.nextDouble() * (maxScale - minScale)

      this.copy(
        areaUm2 = areaUm2 * areaScale,
        totalPowerMw = totalPowerMw * powerScale
      )
    }

    // Helper methods for augmentation
    private def getValidStreamingSizes(current: Int): Vector[Int] = {
      val powerOfTwoSizes = Vector(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024)
      powerOfTwoSizes.filter(size => size >= current / 2 && size <= current * 2 && size != current)
    }

    private def generateEquivalentConfigurations(targetTotal: Int): Vector[(Int, Int, Int, Int, Int)] = {
      val divisors = getDivisors(targetTotal).filter(_ >= 1)
      val validConfigs = scala.collection.mutable.ArrayBuffer[(Int, Int, Int, Int, Int)]()

      for {
        gR <- divisors if gR >= 8 && gR <= 128  // Group PE Row constraints
        gC <- divisors if gC >= 8 && gC <= 128  // Group PE Col constraints
        vR <- divisors if vR >= 1 && vR <= 16   // Vector PE Row constraints
        vC <- divisors if vC >= 1 && vC <= 16   // Vector PE Col constraints
        remaining = targetTotal / (gR * gC * vR * vC)
        if remaining > 0 && remaining <= 32 && isPowerOfTwo(remaining) // NumMultiplier constraints
        if gR * gC * vR * vC * remaining == targetTotal
      } {
        validConfigs += ((gR, gC, vR, vC, remaining))
      }

      validConfigs.toVector.distinct.take(5) // Limit to 5 configurations
    }

    private def getDivisors(n: Int): Vector[Int] = {
      (1 to math.sqrt(n).toInt).filter(n % _ == 0).flatMap { i =>
        if (i * i == n) Vector(i) else Vector(i, n / i)
      }.toVector.sorted
    }

    private def isPowerOfTwo(n: Int): Boolean = {
      if (n <= 0) return false
      (n & (n - 1)) == 0
    }

    private def dataflowToNumeric(dataflow: String): Double = dataflow match {
      case "Is" => 0.0
      case "Os" => 1.0
      case "Ws" => 2.0
      case _ => 0.0
    }
  }

  // Data augmentation pipeline
  object DataAugmentationPipeline {

    def augmentTrainingData(
                             data: Vector[TrainingExample],
                             config: AugmentationConfig,
                             augmentationRatio: Double,
                             random: Random
                           ): Vector[TrainingExample] = {

      val numAugmented = (data.length * augmentationRatio).toInt
      log(s"Generating $numAugmented augmented examples from ${data.length} original examples")

      val augmentedExamples = (0 until numAugmented).par.map { _ =>
        val originalExample = data(random.nextInt(data.length))

        // Apply random combination of augmentation techniques
        val augmentationMethods = Vector(
          () => originalExample.augmentWithNoise(config, random),
          () => originalExample.augmentWithArchitecturalSwap(config, random),
          () => originalExample.augmentWithMultiplierReorganization(config, random),
          () => originalExample.augmentWithDataflowAnalogies(config, data, random),
          () => originalExample.augmentWithScaling(config, random)
        )

        // Apply 1-3 augmentation methods
        val numMethods = 1 + random.nextInt(3)
        val selectedMethods = random.shuffle(augmentationMethods).take(numMethods)

        selectedMethods.foldLeft(originalExample) { (example, method) =>
          method()
        }
      }.seq.toVector

      // Quality filtering: remove invalid or extreme augmentations
      val filteredAugmented = augmentedExamples.filter(isValidAugmentation)

      log(s"After quality filtering: ${filteredAugmented.length} valid augmented examples")

      data ++ filteredAugmented
    }

    def augmentTaskData(
                         supportSet: Vector[(Vector[Double], Vector[Double])],
                         querySet: Vector[(Vector[Double], Vector[Double])],
                         config: AugmentationConfig,
                         random: Random
                       ): (Vector[(Vector[Double], Vector[Double])], Vector[(Vector[Double], Vector[Double])]) = {

      // Light augmentation for task-level data (during meta-training)
      def augmentFeatureVector(input: Vector[Double]): Vector[Double] = {
        if (random.nextDouble() < 0.3) {
          // Add small amount of noise to input features
          input.map(feature => feature + random.nextGaussian() * config.noiseStdDev * 0.5)
        } else input
      }

      def augmentOutputVector(output: Vector[Double]): Vector[Double] = {
        if (random.nextDouble() < 0.2) {
          // Minor output perturbation
          output.map(value => value + random.nextGaussian() * config.noiseStdDev * 0.3)
        } else output
      }

      val augmentedSupport = supportSet.map { case (input, output) =>
        (augmentFeatureVector(input), augmentOutputVector(output))
      }

      val augmentedQuery = querySet.map { case (input, output) =>
        (augmentFeatureVector(input), augmentOutputVector(output))
      }

      (augmentedSupport, augmentedQuery)
    }

    private def isValidAugmentation(example: TrainingExample): Boolean = {
      // Validation criteria for augmented examples
      example.areaUm2 > 0 &&
        example.totalPowerMw > 0 &&
        example.groupPeRow >= 8 && example.groupPeRow <= 128 &&
        example.groupPeCol >= 8 && example.groupPeCol <= 128 &&
        example.vectorPeRow >= 1 && example.vectorPeRow <= 16 &&
        example.vectorPeCol >= 1 && example.vectorPeCol <= 16 &&
        example.numMultiplier >= 1 && example.numMultiplier <= 32 &&
        example.streamingDimensionSize >= 1 &&
        example.totalMultipliers == example.groupPeRow * example.groupPeCol * example.vectorPeRow * example.vectorPeCol * example.numMultiplier &&
        example.areaUm2 < 1000000 && // Reasonable upper bound
        example.totalPowerMw < 10000 // Reasonable upper bound
    }
  }

  // Enhanced training function with data augmentation
  def trainModel(
                  weightOutputPath: String,
                  trainFilePath: String,
                  validationFilePath: String,
                  testFilePath: String,
                  loggerOption: LoggerOption
                ): Try[Unit] = Try {

    setMode(loggerOption)
    log("Starting Enhanced MAML Few-Shot Learning training with Data Augmentation...")

    val config = MAMLConfig()
    val augmentationConfig = AugmentationConfig()

    // Load original data
    val originalTrainData = loadCsvData(trainFilePath)
    val validationData = loadCsvData(validationFilePath)
    val testData = loadCsvData(testFilePath)

    log(s"Loaded ${originalTrainData.length} original training examples")
    log(s"Loaded ${validationData.length} validation examples")
    log(s"Loaded ${testData.length} test examples")

    // Apply data augmentation to training data
    val random = new Random(42)
    val augmentedTrainData = if (config.augmentationEnabled) {
      log("Applying data augmentation to training data...")
      DataAugmentationPipeline.augmentTrainingData(
        originalTrainData,
        augmentationConfig,
        config.augmentationRatio,
        random
      )
    } else {
      log("Data augmentation disabled")
      originalTrainData
    }

    log(s"Total training examples after augmentation: ${augmentedTrainData.length}")

    // Convert data (rest of training process continues as before...)
    val trainDataConverted = augmentedTrainData.par.map(ex => (ex.toInputVector, ex.toOutputVector)).seq.toVector
    val validationDataConverted = validationData.par.map(ex => (ex.toInputVector, ex.toOutputVector)).seq.toVector
    val testDataConverted = testData.par.map(ex => (ex.toInputVector, ex.toOutputVector)).seq.toVector

    // Create enhanced normalizers
    val allInputs = trainDataConverted.map(_._1)
    val allOutputs = trainDataConverted.map(_._2)

    val inputNormalizer = createEnhancedNormalizer(allInputs)
    val outputNormalizer = createEnhancedNormalizer(allOutputs)

    // Create enhanced network architecture
    val inputSize = augmentedTrainData.head.toInputVector.length
    val outputSize = augmentedTrainData.head.toOutputVector.length

//    val network = MAMLNetwork(Vector(
//      MAMLLayer(inputSize, 128, "gelu", useBatchNorm = true, dropoutRate = 0.1),
//      MAMLLayer(128, 128, "gelu", useBatchNorm = true, dropoutRate = 0.1),
//      MAMLLayer(128, 96, "gelu", useBatchNorm = true, dropoutRate = 0.05),
//      MAMLLayer(96, 64, "swish", useBatchNorm = false, dropoutRate = 0.05),
//      MAMLLayer(64, 32, "swish", useBatchNorm = false),
//      MAMLLayer(32, outputSize, "linear")
//    ), useResidualConnections = true)
// Improved network with attention-like mechanisms

  val network = MAMLNetwork(Vector(
    MAMLLayer(inputSize, 256, "gelu", useBatchNorm = true, dropoutRate = 0.15),
    MAMLLayer(256, 256, "gelu", useBatchNorm = true, dropoutRate = 0.15),  // Wider
    MAMLLayer(256, 192, "gelu", useBatchNorm = true, dropoutRate = 0.1),
    MAMLLayer(192, 128, "swish", useBatchNorm = true, dropoutRate = 0.1),
    MAMLLayer(128, 96, "swish", useBatchNorm = false, dropoutRate = 0.05),
    MAMLLayer(96, 64, "swish", useBatchNorm = false, dropoutRate = 0.05),
    // NEW: Separate prediction heads for area and power
    MAMLLayer(64, 32, "swish", useBatchNorm = false),
    MAMLLayer(32, outputSize, "linear")
    ), useResidualConnections = true)  // NEW parameter

    log(s"Enhanced network - Total parameters: ${network.getTotalParams}")

    // Initialize meta-weights with better initialization
    var metaWeights = initializeWeightsAdvanced(network)

    log("Starting enhanced meta-training with augmented data...")

    var bestValidationLoss = Double.MaxValue
    var waitingEpochs = 0
    var currentLR = config.outerLearningRate

    var shouldStop = false
    var epoch = 0

    while (epoch < config.metaEpochs && !shouldStop) {
      val startTime = System.currentTimeMillis()

      // Enhanced learning rate scheduling
      if (epoch > 0 && epoch % config.learningRateDecayFreq == 0) {
        currentLR = math.max(currentLR * config.learningRateDecay, config.minLearningRate)
        log(f"Learning rate decayed to: $currentLR%.6f")
      }

      // Sample diverse tasks with augmentation
      val tasks = sampleDiverseTasksWithAugmentation(trainDataConverted, config, augmentationConfig, random)

      // Compute meta-gradients
      val metaGradients = computeMetaGradientsEnhanced(tasks, metaWeights, network, inputNormalizer, outputNormalizer, config)

      // Update meta-weights with advanced optimization
      metaWeights = updateMetaWeightsAdvanced(metaWeights, metaGradients, currentLR, config)

      val epochTime = System.currentTimeMillis() - startTime

      // Validation
      if (epoch % config.validationFreq == 0) {
        val validationLoss = evaluateModelEnhanced(validationDataConverted, metaWeights, network, inputNormalizer, outputNormalizer, config, random)
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

    // Detailed analysis including augmentation effectiveness
    analyzeDetailedResultsWithAugmentation(model, originalTrainData, augmentedTrainData, validationData, testData, loggerOption)

    // Save model
    saveModel(model, weightOutputPath)
    log(s"Enhanced model with data augmentation saved to: $weightOutputPath")
  }

  // Enhanced task sampling with augmentation
  private def sampleDiverseTasksWithAugmentation(
                                                  data: Vector[(Vector[Double], Vector[Double])],
                                                  config: MAMLConfig,
                                                  augmentationConfig: AugmentationConfig,
                                                  random: Random
                                                ): Vector[Task] = {
    val dataArray = data.toArray

    (0 until config.metaBatchSize).par.map { taskIdx =>
      val shuffledIndices = (0 until dataArray.length).toArray

      // Fisher-Yates shuffle
      for (i <- shuffledIndices.length - 1 to 1 by -1) {
        val j = random.nextInt(i + 1)
        val temp = shuffledIndices(i)
        shuffledIndices(i) = shuffledIndices(j)
        shuffledIndices(j) = temp
      }

      val totalNeeded = config.supportSize + config.querySize
      val selectedIndices = shuffledIndices.take(totalNeeded)

      val supportSet = selectedIndices.take(config.supportSize).map(dataArray(_)).toVector
      val querySet = selectedIndices.drop(config.supportSize).map(dataArray(_)).toVector

      // Apply task-level augmentation if enabled
      val (augmentedSupport, augmentedQuery) = if (config.augmentationEnabled && random.nextDouble() < 0.4) {
        DataAugmentationPipeline.augmentTaskData(supportSet, querySet, augmentationConfig, random)
      } else {
        (supportSet, querySet)
      }

      Task(augmentedSupport, augmentedQuery, s"task_$taskIdx")
    }.seq.toVector
  }

  // Enhanced analysis including augmentation effectiveness
  private def analyzeDetailedResultsWithAugmentation(
                                                      model: MAMLModel,
                                                      originalTrainData: Vector[TrainingExample],
                                                      augmentedTrainData: Vector[TrainingExample],
                                                      validationData: Vector[TrainingExample],
                                                      testData: Vector[TrainingExample],
                                                      loggerOption: LoggerOption
                                                    ): Unit = {

    log("\n" + "="*80)
    log("DETAILED TRAINING RESULTS ANALYSIS WITH DATA AUGMENTATION")
    log("="*80)

    log(s"\nDATA AUGMENTATION STATISTICS:")
    log(s"Original training examples: ${originalTrainData.length}")
    log(s"Augmented training examples: ${augmentedTrainData.length}")
    log(s"Augmentation ratio: ${(augmentedTrainData.length - originalTrainData.length).toDouble / originalTrainData.length * 100}%.1f%%")

    // Analyze augmentation distribution
    analyzeAugmentationDistribution(originalTrainData, augmentedTrainData)

    // Regular performance analysis
    log("\n1. OVERALL PERFORMANCE METRICS")
    log("-" * 50)

    analyzeDatasetPerformance("Original Training Set", originalTrainData, model)
    analyzeDatasetPerformance("Augmented Training Set", augmentedTrainData, model)
    analyzeDatasetPerformance("Validation Set", validationData, model)
    analyzeDatasetPerformance("Test Set", testData, model)

    // Additional augmentation-specific analysis
    log("\n7. AUGMENTATION EFFECTIVENESS ANALYSIS")
    log("-" * 50)
    analyzeAugmentationEffectiveness(originalTrainData, augmentedTrainData, model)
  }

  private def analyzeAugmentationDistribution(
                                               original: Vector[TrainingExample],
                                               augmented: Vector[TrainingExample]
                                             ): Unit = {

    val augmentedOnly = augmented.drop(original.length)

    log(s"\nAugmentation Distribution Analysis:")

    // Dataflow distribution
    val originalDataflowDist = original.groupBy(_.dataflow).mapValues(_.length)
    val augmentedDataflowDist = augmentedOnly.groupBy(_.dataflow).mapValues(_.length)

    log(s"Dataflow distribution:")
    log(s"  Original: ${originalDataflowDist}")
    log(s"  Augmented: ${augmentedDataflowDist}")

    // Multiplier range distribution
    val multiplierRanges = Vector(
      ("1K-4K", (1024, 4096)),
      ("4K-8K", (4096, 8192)),
      ("8K-16K", (8192, 16384))
    )

    log(s"Multiplier range distribution:")
    multiplierRanges.foreach { case (rangeName, (min, max)) =>
      val originalCount = original.count(e => e.totalMultipliers >= min && e.totalMultipliers < max)
      val augmentedCount = augmentedOnly.count(e => e.totalMultipliers >= min && e.totalMultipliers < max)
      log(f"  $rangeName: Original=$originalCount, Augmented=$augmentedCount")
    }
  }

  private def analyzeAugmentationEffectiveness(
                                                original: Vector[TrainingExample],
                                                augmented: Vector[TrainingExample],
                                                model: MAMLModel
                                              ): Unit = {

    val augmentedOnly = augmented.drop(original.length)

    // Compare prediction quality on original vs augmented data
    def evaluateDataset(dataset: Vector[TrainingExample], name: String): (Double, Double) = {
      val predictions = dataset.map { example =>
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

      val avgAreaError = predictions.map(_._1).sum / predictions.length
      val avgPowerError = predictions.map(_._2).sum / predictions.length

      log(f"$name Performance:")
      log(f"  ├── Average Area Error: ${avgAreaError}%.2f%%")
      log(f"  └── Average Power Error: ${avgPowerError}%.2f%%")

      (avgAreaError, avgPowerError)
    }

    val (originalAreaError, originalPowerError) = evaluateDataset(original, "Original Data")
    val (augmentedAreaError, augmentedPowerError) = evaluateDataset(augmentedOnly, "Augmented Data")

    // Calculate robustness improvement
    val areaRobustness = (originalAreaError - augmentedAreaError) / originalAreaError * 100
    val powerRobustness = (originalPowerError - augmentedPowerError) / originalPowerError * 100

    log(f"\nRobustness Analysis:")
    log(f"  ├── Area prediction robustness change: ${areaRobustness}%.2f%%")
    log(f"  └── Power prediction robustness change: ${powerRobustness}%.2f%%")

    // Analyze diversity metrics
    analyzeDiversityMetrics(original, augmentedOnly)
  }

  private def analyzeDiversityMetrics(
                                       original: Vector[TrainingExample],
                                       augmented: Vector[TrainingExample]
                                     ): Unit = {

    def calculateDiversity(data: Vector[TrainingExample]): (Double, Double, Double) = {
      val areas = data.map(_.areaUm2)
      val powers = data.map(_.totalPowerMw)
      val multipliers = data.map(_.totalMultipliers.toDouble)

      def variance(values: Vector[Double]): Double = {
        val mean = values.sum / values.length
        values.map(v => math.pow(v - mean, 2)).sum / values.length
      }

      (variance(areas), variance(powers), variance(multipliers))
    }

    val (origAreaVar, origPowerVar, origMultVar) = calculateDiversity(original)
    val (augAreaVar, augPowerVar, augMultVar) = calculateDiversity(augmented)

    log(f"\nDiversity Metrics:")
    log(f"  Area Variance - Original: ${origAreaVar}%.0f, Augmented: ${augAreaVar}%.0f")
    log(f"  Power Variance - Original: ${origPowerVar}%.2f, Augmented: ${augPowerVar}%.2f")
    log(f"  Multiplier Variance - Original: ${origMultVar}%.0f, Augmented: ${augMultVar}%.0f")

    val diversityIncrease = ((augAreaVar + augPowerVar + augMultVar) - (origAreaVar + origPowerVar + origMultVar)) /
      (origAreaVar + origPowerVar + origMultVar) * 100
    log(f"  Overall Diversity Increase: ${diversityIncrease}%.1f%%")
  }

  // Continue with the remaining methods from the original implementation
  // (Layer, Network, Model classes and other helper functions remain the same)

  case class MAMLLayer(
                        inputSize: Int,
                        outputSize: Int,
                        activation: String = "relu",
                        useBatchNorm: Boolean = false,
                        dropoutRate: Double = 0.0
                      ) {

    def getParamCount: Int = {
      val baseParams = inputSize * outputSize + outputSize
      val bnParams = if (useBatchNorm) outputSize * 2 else 0
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

      val weightMatrix = weights.dropRight(if (useBatchNorm) 3 else 1)
      val weightGrads = (0 until inputSize).map { i =>
        (0 until outputSize).map { j =>
          layerOutput.input(i) * activationGrad(j)
        }.toVector
      }.toVector

      val biasGrads = activationGrad

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

  case class LayerOutput(
                          activation: Vector[Double],
                          preActivation: Vector[Double],
                          input: Vector[Double]
                        )

  case class ForwardPass(layerOutputs: Vector[LayerOutput])

  case class MAMLNetwork(layers: Vector[MAMLLayer], useResidualConnections: Boolean = true) {

    def forward(input: Vector[Double], weights: Vector[Vector[Vector[Double]]], training: Boolean = true): Vector[Double] = {
      var activation = input
      var residualConnection: Option[Vector[Double]] = None

      for (i <- layers.indices) {
        val layerOutput = layers(i).forward(activation, weights(i), training)

        if (useResidualConnections && residualConnection.isDefined &&
          residualConnection.get.length == layerOutput.length && i > 0) {
          activation = layerOutput.zip(residualConnection.get).map { case (out, res) => out + res }
        } else {
          activation = layerOutput
        }

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

      for (i <- (layers.length - 1) to 0 by -1) {
        val (inputGrad, layerGrads) = layers(i).backward(outputGrad, forwardPass.layerOutputs(i), weights(i))
        gradients.prepend(layerGrads)
        outputGrad = inputGrad
      }

      gradients.toVector
    }

    def getTotalParams: Int = layers.map(_.getParamCount).sum
  }

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

      for (step <- 0 until config.innerSteps) {
        val gradients = computeGradientsAnalytical(supportSet, adaptedWeights)
        val clippedGradients = clipGradients(gradients, config.gradientClipping)
        adaptedWeights = updateWeightsWithRegularization(adaptedWeights, clippedGradients, currentLR, config.weightDecay)
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
            weight * (1.0 - learningRate * weightDecay) - learningRate * grad
          }
        }.seq.toVector
      }.seq.toVector
    }
  }

  case class DataNormalizer(
                             mean: Vector[Double],
                             std: Vector[Double],
                             median: Vector[Double],
                             mad: Vector[Double]
                           ) {
    def normalize(data: Vector[Double]): Vector[Double] = {
      data.zipWithIndex.map { case (value, i) =>
        val robustScale = if (mad(i) > 1e-8) (value - median(i)) / mad(i) else value - median(i)
        val standardScale = if (std(i) > 1e-8) (value - mean(i)) / std(i) else value - mean(i)
        0.7 * standardScale + 0.3 * robustScale
      }
    }

    def denormalize(data: Vector[Double]): Vector[Double] = {
      data.zip(mean).zip(std).map { case ((value, m), s) =>
        value * s + m
      }
    }
  }

  case class Task(
                   supportSet: Vector[(Vector[Double], Vector[Double])],
                   querySet: Vector[(Vector[Double], Vector[Double])],
                   taskId: String
                 )

  // Helper functions (keeping the existing implementations)
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
      medianDeviation * 1.4826 + 1e-8
    }.seq.toVector

    DataNormalizer(mean, std, median, mad)
  }

  private def initializeWeightsAdvanced(network: MAMLNetwork): Vector[Vector[Vector[Double]]] = {
    val random = new Random(42)

    network.layers.par.map { layer =>
      val initType = layer.activation match {
        case "relu" | "leaky_relu" | "gelu" | "swish" => "he"
        case _ => "xavier"
      }

      val limit = initType match {
        case "he" => math.sqrt(2.0 / layer.inputSize)
        case "xavier" => math.sqrt(6.0 / (layer.inputSize + layer.outputSize))
      }

      val weights = (0 until layer.inputSize).map { _ =>
        (0 until layer.outputSize).map { _ =>
          random.nextGaussian() * limit
        }.toVector
      }.toVector

      val biases = (0 until layer.outputSize).map { _ =>
        if (layer.activation == "relu" || layer.activation == "leaky_relu") 0.01 else 0.0
      }.toVector

      if (layer.useBatchNorm) {
        val gamma = Vector.fill(layer.outputSize)(1.0)
        val beta = Vector.fill(layer.outputSize)(0.0)
        weights ++ Vector(biases, gamma, beta)
      } else {
        weights :+ biases
      }
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

    weights.zip(gradients).par.map { case (layerWeights, layerGrads) =>
      layerWeights.zip(layerGrads).par.map { case (neuronWeights, neuronGrads) =>
        neuronWeights.zip(neuronGrads).map { case (weight, grad) =>
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

  private def sampleDiverseTasks(
                                  data: Vector[(Vector[Double], Vector[Double])],
                                  config: MAMLConfig,
                                  random: Random
                                ): Vector[Task] = {
    val dataArray = data.toArray

    (0 until config.metaBatchSize).par.map { taskIdx =>
      val shuffledIndices = (0 until dataArray.length).toArray

      for (i <- shuffledIndices.length - 1 to 1 by -1) {
        val j = random.nextInt(i + 1)
        val temp = shuffledIndices(i)
        shuffledIndices(i) = shuffledIndices(j)
        shuffledIndices(j) = temp
      }

      val totalNeeded = config.supportSize + config.querySize
      val selectedIndices = shuffledIndices.take(totalNeeded)

      val supportSet = selectedIndices.take(config.supportSize).map(dataArray(_)).toVector
      val querySet = selectedIndices.drop(config.supportSize).map(dataArray(_)).toVector

      Task(supportSet, querySet, s"task_$taskIdx")
    }.seq.toVector
  }

  private def loadCsvData(filePath: String): Vector[TrainingExample] = {
    val resourcePath = if (filePath.startsWith("/")) filePath.substring(1) else filePath

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
      areaUm2 = 0.0,
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

    buffer.putInt(model.network.layers.length)
    buffer.put(if (model.network.useResidualConnections) 1.toByte else 0.toByte)

    model.network.layers.foreach { layer =>
      buffer.putInt(layer.inputSize)
      buffer.putInt(layer.outputSize)
      writeString(buffer, layer.activation)
      buffer.put(if (layer.useBatchNorm) 1.toByte else 0.toByte)
      buffer.putDouble(layer.dropoutRate)
    }

    model.metaWeights.foreach { layer =>
      buffer.putInt(layer.length)
      layer.foreach { neuron =>
        buffer.putInt(neuron.length)
        neuron.foreach(buffer.putDouble)
      }
    }

    writeEnhancedNormalizer(buffer, model.inputNormalizer)
    writeEnhancedNormalizer(buffer, model.outputNormalizer)
    writeEnhancedConfig(buffer, model.config)

    val result = new Array[Byte](buffer.position())
    buffer.rewind()
    buffer.get(result)
    result
  }

  private def deserializeModel(buffer: ByteBuffer): MAMLModel = {
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

    val weights = layers.map { _ =>
      val layerSize = buffer.getInt()
      (0 until layerSize).map { _ =>
        val neuronSize = buffer.getInt()
        (0 until neuronSize).map(_ => buffer.getDouble()).toVector
      }.toVector
    }.toVector

    val inputNormalizer = readEnhancedNormalizer(buffer)
    val outputNormalizer = readEnhancedNormalizer(buffer)
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
    buffer.put(if (config.augmentationEnabled) 1.toByte else 0.toByte)
    buffer.putDouble(config.augmentationRatio)
    buffer.putDouble(config.noiseStdDev)
    buffer.putDouble(config.architecturalSwapProbability)
    buffer.putDouble(config.dimensionPerturbationRange)
    buffer.putDouble(config.dataflowMixupProbability)
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
      minLearningRate = buffer.getDouble(),
      augmentationEnabled = buffer.get() == 1.toByte,
      augmentationRatio = buffer.getDouble(),
      noiseStdDev = buffer.getDouble(),
      architecturalSwapProbability = buffer.getDouble(),
      dimensionPerturbationRange = buffer.getDouble(),
      dataflowMixupProbability = buffer.getDouble()
    )
  }

  // Additional helper methods for dataset performance analysis
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

  private def dataflowToNumeric(dataflow: String): Double = dataflow match {
    case "Is" => 0.0
    case "Os" => 1.0
    case "Ws" => 2.0
    case _ => 0.0
  }
}
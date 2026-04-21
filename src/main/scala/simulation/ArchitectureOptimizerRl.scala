package simulation

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import common.Dataflow
import scala.collection.parallel.CollectionConverters._
import scala.util.Random

/**
 * RL hyperparameter configuration for Q-learning based architecture optimization.
 *
 * @param learningRate        alpha: how much new experience overrides old Q-values
 * @param discountFactor      gamma: importance of future rewards
 * @param initialEpsilon      starting exploration rate
 * @param epsilonDecay        multiplicative decay per episode
 * @param minEpsilon          floor for exploration rate
 * @param initialQValue       optimistic initialization to encourage exploration
 * @param numTrainingEpisodes number of training episodes per architecture candidate
 * @param maxStepsPerEpisode  maximum steps allowed in a single episode
 * @param rewardScale         multiplier for positive improvements in reward
 * @param penaltyScale        multiplier for negative improvements (regression) in reward
 * @param invalidPenalty      penalty for infeasible configurations
 * @param stepCost            small penalty per step to encourage efficiency
 * @param earlyStopPatience   stop training if no improvement for this many consecutive episodes
 */
case class RlConfig(
                     learningRate: Double = 0.3,
                     discountFactor: Double = 0.95,
                     initialEpsilon: Double = 0.8,
                     epsilonDecay: Double = 0.85,
                     minEpsilon: Double = 0.05,
                     initialQValue: Double = 0.1,
                     numTrainingEpisodes: Int = 5,
                     maxStepsPerEpisode: Int = 10,
                     rewardScale: Double = 5.0,
                     penaltyScale: Double = 2.0,
                     invalidPenalty: Double = -1.0,
                     stepCost: Double = -0.01,
                     earlyStopPatience: Int = 2,
                     maxCandidatesForRl: Int = 80
                   )

class ArchitectureOptimizerRl(
                               val simConfig: SystemArchitectureOptimizerRl.SimulationConfig,
                               val architectureCandidates: ArrayBuffer[Architecture],
                               val minSramSize: Int,
                               val loggerOption: LoggerOption,
                               val rlConfig: RlConfig = RlConfig(),
                             ) extends Logger {

  setMode(loggerOption)

  private val executableCandidates = ArrayBuffer.empty[Architecture]
  private val calculatedAllResults = ArrayBuffer.empty[ArchitectureResult]
  private val rankedCalculatedResults = ArrayBuffer.empty[ArchitectureResult]
  private val archOptimizedResults = ArrayBuffer.empty[ArchitectureResult]
  private val singleSramOptimizedResults = ArrayBuffer.empty[ArchitectureResult]
  private val rankedSingleSramOptimizedResults = ArrayBuffer.empty[ArchitectureResult]

  private val processOneMargin: Double = 20.0
  private val processTwoMargin: Double = 40.0
  private val processThreeMargin: Double = 10.0

  def run(): Unit = {

    log("[Optimization Process]")
    executableCandidates ++= validateCandidates(architectureCandidates)
    logExecutableArch(
      candidates = architectureCandidates,
      validCandidates = executableCandidates,
    )

    calculatedAllResults ++= process1(executableCandidates)
    rankedCalculatedResults ++= rankResults(calculatedAllResults, processOneMargin)
    logProcessOne(
      validCandidates = executableCandidates,
      simulatedCandidates = calculatedAllResults,
      rankedSimulatedCandidates = rankedCalculatedResults,
    )

    // Process 2: RL-based architecture optimization
    archOptimizedResults ++= process2(rankedCalculatedResults, processTwoMargin)

    // Process 3: single SRAM optimization
    singleSramOptimizedResults ++= process3(archOptimizedResults)
    rankedSingleSramOptimizedResults ++= rankResults(singleSramOptimizedResults, processThreeMargin)
    logProcessThree(
      preProcess = archOptimizedResults,
      postProcess = singleSramOptimizedResults,
      rankedPostProcess = rankedSingleSramOptimizedResults
    )

    logOptimizationSummary()
  }

  // ============================================================
  // Reinforcement Learning Components for Process 2
  // ============================================================

  /**
   * RL State: encoded as (sramSizeLog2, streamingDimLog2).
   * Both parameters are powers of 2, so log2 gives a compact integer representation.
   */
  private case class RlState(sramSizeLog2: Int, streamingDimLog2: Int)

  private sealed trait RlAction
  private case object ReduceSram extends RlAction
  private case object ReduceStreamDim extends RlAction
  private case object ReduceBoth extends RlAction
  private case object Stop extends RlAction

  private val allActions: Seq[RlAction] = Seq(ReduceSram, ReduceStreamDim, ReduceBoth, Stop)

  private class QTable(config: RlConfig) {
    private val table = mutable.HashMap.empty[(RlState, RlAction), Double]
    private var epsilon: Double = config.initialEpsilon
    private val rng = new Random(42)
    private var totalUpdates: Int = 0

    /** Get Q-value for a state-action pair. Optimistic initialization. */
    def getQ(state: RlState, action: RlAction): Double = {
      table.getOrElse((state, action), config.initialQValue)
    }

    /** Update Q-value using the standard Q-learning update rule. */
    def update(state: RlState, action: RlAction, reward: Double, nextState: RlState, done: Boolean): Unit = {
      val currentQ = getQ(state, action)
      val maxNextQ = if (done) 0.0
      else allActions.map(a => getQ(nextState, a)).max
      val target = reward + config.discountFactor * maxNextQ
      val newQ = currentQ + config.learningRate * (target - currentQ)
      table((state, action)) = newQ
      totalUpdates += 1
    }

    /**
     * Select an action using epsilon-greedy policy.
     * Masks out invalid actions (e.g., can't reduce below minimum).
     */
    def selectAction(state: RlState, validActions: Seq[RlAction]): RlAction = {
      if (validActions.isEmpty) return Stop

      if (rng.nextDouble() < epsilon) {
        // Explore: random valid action
        validActions(rng.nextInt(validActions.size))
      } else {
        // Exploit: best Q-value among valid actions
        validActions.maxBy(a => getQ(state, a))
      }
    }

    /** Decay epsilon after each episode. */
    def decayEpsilon(): Unit = {
      epsilon = math.max(config.minEpsilon, epsilon * config.epsilonDecay)
    }

    /** Get current epsilon for logging. */
    def currentEpsilon: Double = epsilon

    /** Get total number of Q-value updates for logging. */
    def getTotalUpdates: Int = totalUpdates

    /** Get the number of unique state-action pairs visited. */
    def tableSize: Int = table.size

    /**
     * Get the greedy policy action for a given state (for exploitation after training).
     */
    def greedyAction(state: RlState, validActions: Seq[RlAction]): RlAction = {
      if (validActions.isEmpty) Stop
      else validActions.maxBy(a => getQ(state, a))
    }
  }

  /**
   * Environment that wraps the architecture simulation.
   * Handles state transitions, reward computation, and validity checks.
   */
  private class ArchitectureEnvironment(
                                         baseArch: Architecture,
                                         minSramLog2: Int,
                                         maxSramLog2: Int,
                                         minStreamDimLog2: Int,
                                         maxStreamDimLog2: Int,
                                         maximize: Boolean
                                       ) {
    // Cache simulation results to avoid redundant evaluations
    private val simulationCache = mutable.HashMap.empty[RlState, ArchitectureResult]

    /** Get valid actions from a given state. */
    def getValidActions(state: RlState): Seq[RlAction] = {
      val actions = ArrayBuffer.empty[RlAction]
      actions += Stop // always available

      val canReduceSram = state.sramSizeLog2 > minSramLog2
      val canReduceStream = state.streamingDimLog2 > minStreamDimLog2

      if (canReduceSram) actions += ReduceSram
      if (canReduceStream) actions += ReduceStreamDim
      if (canReduceSram && canReduceStream) actions += ReduceBoth

      actions.toSeq
    }

    /** Apply an action to get the next state. Returns None if action is Stop. */
    def nextState(state: RlState, action: RlAction): Option[RlState] = action match {
      case ReduceSram =>
        Some(RlState(state.sramSizeLog2 - 1, state.streamingDimLog2))
      case ReduceStreamDim =>
        Some(RlState(state.sramSizeLog2, state.streamingDimLog2 - 1))
      case ReduceBoth =>
        Some(RlState(state.sramSizeLog2 - 1, state.streamingDimLog2 - 1))
      case Stop =>
        None
    }

    /** Evaluate an architecture at a given state, with caching. */
    def evaluate(state: RlState): ArchitectureResult = {
      simulationCache.getOrElseUpdate(state, {
        val arch = buildArchFromState(state)
        buildAndRunSimulation(arch)
      })
    }

    /** Build an Architecture from the RL state. */
    private def buildArchFromState(state: RlState): Architecture = {
      val sramSize = 1 << state.sramSizeLog2
      val streamDim = 1 << state.streamingDimLog2

      val updatedArrayConfig = baseArch.arrayConfig.copy(
        arraySynthesisData = FewShotPredictor.predict(
          FewShotPredictor.InputFeatures(
            dataflow = baseArch.arrayConfig.dataflow.toString.toUpperCase,
            totalNumberOfMultipliers = baseArch.arrayConfig.totalNumberOfMultipliers,
            r = baseArch.arrayConfig.groupPeRow,
            c = baseArch.arrayConfig.groupPeCol,
            a = baseArch.arrayConfig.vectorPeRow,
            b = baseArch.arrayConfig.vectorPeCol,
            p = baseArch.arrayConfig.numMultiplier,
            streamingDimensionSize = streamDim
          )
        ).toOption
      )

      baseArch
        .withUniformSramSizes(sramSize)
        .withStreamingDimensionSize(streamDim)
        .copy(arrayConfig = updatedArrayConfig)
    }

    def computeReward(
                       currentResult: ArchitectureResult,
                       newResult: ArchitectureResult
                     ): Double = {

      val isValid = newResult.simulationResult.cycle != Long.MaxValue
      if (!isValid) return rlConfig.invalidPenalty + rlConfig.stepCost

      val currentObj = extractObjective(currentResult.simulationResult)
      val newObj = extractObjective(newResult.simulationResult)

      // Guard against zero/near-zero baseline: use absolute difference with small scaling
      val epsilon = 1e-12
      val improvement = if (maximize) {
        if (math.abs(currentObj) > epsilon) (newObj - currentObj) / math.abs(currentObj)
        else if (newObj > currentObj) math.min((newObj - currentObj) * 0.1, 0.5)
        else if (newObj < currentObj) math.max((newObj - currentObj) * 0.1, -0.5)
        else 0.0
      } else {
        if (math.abs(currentObj) > epsilon) (currentObj - newObj) / math.abs(currentObj)
        else if (newObj < currentObj) math.min((currentObj - newObj) * 0.1, 0.5)
        else if (newObj > currentObj) math.max((currentObj - newObj) * 0.1, -0.5)
        else 0.0
      }

      // Scale the reward: positive improvement gets amplified
      val reward = if (improvement > 0) {
        improvement * rlConfig.rewardScale
      } else {
        improvement * rlConfig.penaltyScale
      }

      reward + rlConfig.stepCost
    }

    def getCacheSize: Int = simulationCache.size
  }

  private def optimizeSramStreamingTradeOffsRl(
                                                archResultBuffer: ArrayBuffer[ArchitectureResult],
                                                processMargin: Double
                                              ): ArrayBuffer[ArchitectureResult] = {

    val previousResults = archResultBuffer.clone()
    val maximize = isMaximizing

    val numTrainingEpisodes = rlConfig.numTrainingEpisodes

    // Limit candidate count to avoid excessive simulation calls
    val candidatesToOptimize = if (archResultBuffer.size > rlConfig.maxCandidatesForRl) {
      log(s"\t\t[Candidate Pruning] ${archResultBuffer.size} -> ${rlConfig.maxCandidatesForRl} (top candidates only)")
      archResultBuffer.take(rlConfig.maxCandidatesForRl)
    } else {
      archResultBuffer
    }

    // Phase 1: Training — independent Q-tables per architecture, fully parallelizable
    // Each architecture has its own Q-table since base configs differ and
    // state semantics (log2 offsets) are relative to each architecture's bounds.
    log(s"\t\t[RL Training Phase]")
    log(s"\t\t\tHyperparameters: lr=${rlConfig.learningRate}, gamma=${rlConfig.discountFactor}, " +
      s"eps=${rlConfig.initialEpsilon}->${rlConfig.minEpsilon}, episodes=$numTrainingEpisodes, " +
      s"earlyStop=${rlConfig.earlyStopPatience}, candidates=${candidatesToOptimize.size}")

    case class TrainingUnit(
                             env: ArchitectureEnvironment,
                             qTable: QTable,
                             initState: RlState,
                             initialResult: ArchitectureResult,
                             effectiveMaxSteps: Int
                           )

    val trainingUnits = candidatesToOptimize.map { initialResult =>
      val baseArch = initialResult.architecture
      val maxSramLog2 = log2Floor(baseArch.singleBufferLimitKbA)
      val minSramLog2 = log2Ceil(math.max(minSramSize, 1))
      val maxStreamDimLog2 = log2Floor(baseArch.streamingDimensionSize)
      val minStreamDimLog2 = 0

      val env = new ArchitectureEnvironment(
        baseArch, minSramLog2, maxSramLog2, minStreamDimLog2, maxStreamDimLog2, maximize
      )
      val initState = RlState(maxSramLog2, maxStreamDimLog2)
      val qTable = new QTable(rlConfig)

      // Dynamic max steps: actual search depth = sum of reducible dimensions
      val searchDepth = (maxSramLog2 - minSramLog2) + (maxStreamDimLog2 - minStreamDimLog2)
      val effectiveMaxSteps = math.min(math.max(searchDepth, 2), rlConfig.maxStepsPerEpisode)

      TrainingUnit(env, qTable, initState, initialResult, effectiveMaxSteps)
    }

    // Training loop: each architecture trains independently in parallel
    // with early stopping when no improvement is observed
    val trainingResults = trainingUnits.par.map { unit =>
      var bestReward = Double.MinValue
      var noImprovementCount = 0
      var actualEpisodes = 0

      for (episode <- 0 until numTrainingEpisodes if noImprovementCount < rlConfig.earlyStopPatience) {
        val (episodeReward, _) = runEpisode(unit.qTable, unit.env, unit.initState, unit.initialResult,
          training = true, maxSteps = unit.effectiveMaxSteps)
        unit.qTable.decayEpsilon()
        actualEpisodes = episode + 1

        if (episodeReward > bestReward + 1e-6) {
          bestReward = episodeReward
          noImprovementCount = 0
        } else {
          noImprovementCount += 1
        }
      }

      (unit, actualEpisodes)
    }.seq

    val totalEpisodes = trainingResults.map(_._2).sum
    val totalQUpdates = trainingResults.map(_._1.qTable.getTotalUpdates).sum
    val totalTableSize = trainingResults.map(_._1.qTable.tableSize).sum

    log(s"\t\t\tTraining complete: $totalQUpdates Q-value updates, " +
      s"$totalTableSize state-action pairs explored, " +
      s"$totalEpisodes total episodes (avg ${totalEpisodes / math.max(trainingResults.size, 1)} per arch)")

    // Phase 2: Exploitation — use greedy policy to optimize each architecture
    log(s"\t\t[RL Exploitation Phase]")

    val optimizedResults = trainingResults.par.map { case (unit, _) =>
      exploitPolicy(unit.qTable, unit.env, unit.initState, unit.initialResult, unit.effectiveMaxSteps)
    }.seq

    val resultBuffer = ArrayBuffer.empty[ArchitectureResult]
    resultBuffer ++= optimizedResults

    val rankedResults = rankResults(resultBuffer, processMargin)

    logProcessTwo(
      preProcess = previousResults,
      postProcess = resultBuffer,
      rankedPostProcess = rankedResults,
      iteration = 0
    )

    log("")
    rankedResults
  }

  /**
   * Run a single training episode.
   * Returns (total reward, number of steps).
   */
  private def runEpisode(
                          qTable: QTable,
                          env: ArchitectureEnvironment,
                          initState: RlState,
                          initialResult: ArchitectureResult,
                          training: Boolean,
                          maxSteps: Int = -1
                        ): (Double, Int) = {

    val effectiveMaxSteps = if (maxSteps > 0) maxSteps else rlConfig.maxStepsPerEpisode
    var currentState = initState
    var currentResult = initialResult
    var totalReward = 0.0
    var step = 0
    var done = false

    while (!done && step < effectiveMaxSteps) {
      val validActions = env.getValidActions(currentState)

      val action = if (training) {
        qTable.selectAction(currentState, validActions)
      } else {
        qTable.greedyAction(currentState, validActions)
      }

      if (action == Stop) {
        done = true
      } else {
        env.nextState(currentState, action) match {
          case Some(nextSt) =>
            val nextResult = env.evaluate(nextSt)
            val reward = env.computeReward(currentResult, nextResult)

            val isValid = nextResult.simulationResult.cycle != Long.MaxValue

            if (training) {
              // For invalid transitions, mark as terminal so the agent learns
              // this action leads to a dead end from this state
              qTable.update(currentState, action, reward, nextSt, done = !isValid)
            }

            totalReward += reward

            if (isValid) {
              currentState = nextSt
              currentResult = nextResult
            }
          // If invalid, stay in current state but the Q-update above
          // correctly marks this as a terminal transition

          case None =>
            done = true
        }
      }

      step += 1
    }

    // Terminal update: reward of 0 for stopping
    if (training) {
      qTable.update(currentState, Stop, 0.0, currentState, done = true)
    }

    (totalReward, step)
  }

  /**
   * Exploit the learned policy: follow greedy actions from the initial state,
   * tracking the best result seen along the trajectory.
   */
  private def exploitPolicy(
                             qTable: QTable,
                             env: ArchitectureEnvironment,
                             initState: RlState,
                             initialResult: ArchitectureResult,
                             effectiveMaxSteps: Int = -1
                           ): ArchitectureResult = {

    var currentState = initState
    var currentResult = initialResult
    var bestResult = initialResult
    var done = false
    var step = 0
    val maxSteps = if (effectiveMaxSteps > 0) effectiveMaxSteps else rlConfig.maxStepsPerEpisode

    while (!done && step < maxSteps) {
      val validActions = env.getValidActions(currentState)
      val action = qTable.greedyAction(currentState, validActions)

      if (action == Stop) {
        done = true
      } else {
        env.nextState(currentState, action) match {
          case Some(nextSt) =>
            val nextResult = env.evaluate(nextSt)
            val isValid = nextResult.simulationResult.cycle != Long.MaxValue

            if (isValid) {
              currentState = nextSt
              currentResult = nextResult

              if (isImproved(bestResult.simulationResult, nextResult.simulationResult)) {
                bestResult = nextResult
              }
            } else {
              done = true // hit infeasible region, stop
            }

          case None =>
            done = true
        }
      }
      step += 1
    }

    bestResult
  }

  // ============================================================
  // Utility methods
  // ============================================================

  private def log2Floor(x: Int): Int = {
    if (x <= 0) 0
    else 31 - Integer.numberOfLeadingZeros(x)
  }

  private def log2Ceil(x: Int): Int = {
    if (x <= 0) 0
    else 32 - Integer.numberOfLeadingZeros(x - 1)
  }

  private def extractObjective(result: SimulationResult): Double = {
    simConfig.metric match {
      case SystemArchitectureOptimizerRl.OptimizationMetric.Cycle => result.cycle.toDouble
      case SystemArchitectureOptimizerRl.OptimizationMetric.Energy => result.energyPj.getOrElse(Double.MaxValue)
      case SystemArchitectureOptimizerRl.OptimizationMetric.Area => result.areaUm2.getOrElse(Double.MaxValue)
      case SystemArchitectureOptimizerRl.OptimizationMetric.TOPS => result.tops.getOrElse(0.0)
    }
  }

  private def isMaximizing: Boolean = {
    simConfig.metric == SystemArchitectureOptimizerRl.OptimizationMetric.TOPS
  }

  private def isImproved(currentResult: SimulationResult, newResult: SimulationResult): Boolean = {
    simConfig.metric match {
      case SystemArchitectureOptimizerRl.OptimizationMetric.Cycle =>
        newResult.cycle < currentResult.cycle
      case SystemArchitectureOptimizerRl.OptimizationMetric.Energy =>
        newResult.energyPj.exists(e => currentResult.energyPj.exists(ce => e < ce))
      case SystemArchitectureOptimizerRl.OptimizationMetric.Area =>
        newResult.areaUm2.exists(a => currentResult.areaUm2.exists(ca => a < ca))
      case SystemArchitectureOptimizerRl.OptimizationMetric.TOPS =>
        newResult.tops.exists(a => currentResult.tops.exists(ca => a > ca))
    }
  }

  // ============================================================
  // Process pipeline methods (Process 1, 2, 3)
  // ============================================================

  private def validateCandidates(archBuffer: ArrayBuffer[Architecture]): ArrayBuffer[Architecture] = {
    log("\t[Check Initial Streaming Dimension]")
    println("Check Initial Streaming Dimension")
    checkInitialStreamingDimensions(archBuffer)
  }

  private def process1(archBuffer: ArrayBuffer[Architecture]): ArrayBuffer[ArchitectureResult] = {
    log("\t[Process1: Find High Efficiency STA configs]")
    println("Process1: Find High Efficiency STA configs")
    evaluateAllArchitectures(archBuffer)
  }

  private def process2(archResultBuffer: ArrayBuffer[ArchitectureResult], processMargin: Double): ArrayBuffer[ArchitectureResult] = {
    log(s"\t[Process2: RL-based Architecture Optimization]")
    println("Process2: RL-based Architecture Optimization")
    optimizeSramStreamingTradeOffsRl(archResultBuffer, processMargin)
  }

  private def process3(archResultBuffer: ArrayBuffer[ArchitectureResult]): ArrayBuffer[ArchitectureResult] = {
    log(s"\t[Process3: Optimize Single SRAM]")
    println("Process3: Optimize Single SRAM")
    halfSingleSramSizes(archResultBuffer)
  }

  // ============================================================
  // Logging methods (same as original)
  // ============================================================

  def logTopResults(): Unit = {
    log(s"[Show Top Results]")
    rankedSingleSramOptimizedResults.foreach(logSummary)
    log("")
  }

  def logTopResultsCsv(): Unit = {
    log(s"[Show Top Results CSV Format (" +
      s"Dataflow, " +
      s"STA Config, " +
      s"Cycle, " +
      s"Area, " +
      s"Energy, " +
      s"TOPS/W/mm^2, " +
      s"Streaming Dimension, " +
      s"Single Buffer Size A, " +
      s"Single Buffer Size B, " +
      s"Single Buffer Size C, " +
      s"Memory Utilization]")
    rankedSingleSramOptimizedResults.foreach(logCsv)
    log("")
  }

  def logTopEasyResultsCsv(): Unit = {
    log(s"[Show Top Results CSV Format (" +
      s"Dataflow, " +
      s"STA Config)] ")
    rankedSingleSramOptimizedResults.foreach(logEasyCsv)
    log("")
  }

  private def logExecutableArch(
                                 candidates: ArrayBuffer[Architecture],
                                 validCandidates: ArrayBuffer[Architecture]
                               ): Unit = {
    log(s"\t\t${candidates.size} architectures were candidates")
    log(s"\t\t${validCandidates.size} architectures were valid")
    log("")
  }

  private def logProcessOne(
                             validCandidates: ArrayBuffer[Architecture],
                             simulatedCandidates: ArrayBuffer[ArchitectureResult],
                             rankedSimulatedCandidates: ArrayBuffer[ArchitectureResult],
                           ): Unit = {
    log(s"\t\t${validCandidates.size} valid architectures were passed to process")
    log(s"\t\t${simulatedCandidates.size} architectures were successfully processed")
    log(s"\t\t${rankedSimulatedCandidates.size} architectures meet performance criteria")
    log("")
  }

  private def logProcessTwo(
                             preProcess: ArrayBuffer[ArchitectureResult],
                             postProcess: ArrayBuffer[ArchitectureResult],
                             rankedPostProcess: ArrayBuffer[ArchitectureResult],
                             iteration: Int,
                           ): Unit = {

    log(s"\t\t[RL Optimization Result]")
    log(s"\t\t\t${preProcess.size} architectures were processed")
    log(s"\t\t\t${postProcess.size} optimized architectures were evaluated")
    log(s"\t\t\t${rankedPostProcess.size} architectures meet performance criteria")

    if (preProcess.nonEmpty && rankedPostProcess.nonEmpty) {
      val preProcessBest = preProcess.head
      val postProcessBest = rankedPostProcess.head

      val cycleImprovement = (preProcessBest.simulationResult.cycle - postProcessBest.simulationResult.cycle).toDouble /
        preProcessBest.simulationResult.cycle * 100
      log(s"\t\t\tCycle time: ${String.format("%.2f", cycleImprovement)}% improvement")

      val avgSramSizeBefore = (preProcessBest.architecture.singleBufferLimitKbA +
        preProcessBest.architecture.singleBufferLimitKbB +
        preProcessBest.architecture.singleBufferLimitKbC) / 3.0

      val avgSramSizeAfter = (postProcessBest.architecture.singleBufferLimitKbA +
        postProcessBest.architecture.singleBufferLimitKbB +
        postProcessBest.architecture.singleBufferLimitKbC) / 3.0

      val sramReduction = (avgSramSizeBefore - avgSramSizeAfter) / avgSramSizeBefore * 100
      log(s"\t\t\tAverage SRAM size: ${String.format("%.2f", sramReduction)}% reduction")

      if (preProcessBest.architecture.streamingDimensionSize != postProcessBest.architecture.streamingDimensionSize) {
        val streamDimChange = (preProcessBest.architecture.streamingDimensionSize - postProcessBest.architecture.streamingDimensionSize).toDouble /
          preProcessBest.architecture.streamingDimensionSize * 100
        log(s"\t\t\tStreaming dimension: ${String.format("%.2f", streamDimChange)}% reduction")
      }

      val initialAvgUtilization = preProcessBest.simulationResult.averageMemoryUtilization
      val finalAvgUtilization = postProcessBest.simulationResult.averageMemoryUtilization
      val utilizationImprovement = finalAvgUtilization - initialAvgUtilization
      log(s"\t\t\tAverage memory utilization ${String.format("%.2f", utilizationImprovement)}% increase")

      if (preProcessBest.simulationResult.energyPj.isDefined && postProcessBest.simulationResult.energyPj.isDefined) {
        val energyImprovement = (preProcessBest.simulationResult.energyPj.get - postProcessBest.simulationResult.energyPj.get) /
          preProcessBest.simulationResult.energyPj.get * 100
        log(s"\t\t\tEnergy: ${String.format("%.2f", energyImprovement)}% improvement")
      }

      if (preProcessBest.simulationResult.areaUm2.isDefined && postProcessBest.simulationResult.areaUm2.isDefined) {
        val areaImprovement = (preProcessBest.simulationResult.areaUm2.get - postProcessBest.simulationResult.areaUm2.get) /
          preProcessBest.simulationResult.areaUm2.get * 100
        log(s"\t\t\tArea: ${String.format("%.2f", areaImprovement)}% improvement")
      }

      if (preProcessBest.simulationResult.tops.isDefined && postProcessBest.simulationResult.tops.isDefined) {
        val oldValue = preProcessBest.simulationResult.tops.get
        val newValue = postProcessBest.simulationResult.tops.get
        if (oldValue > 0 && newValue > 0) {
          val improvement = (newValue - oldValue) / oldValue * 100
          log(s"\t\t\tTOPS/W/mm² Efficiency: ${String.format("%.2f", improvement)}% improvement")
        }
      }
    }
    log("")
  }

  private def logProcessThree(
                               preProcess: ArrayBuffer[ArchitectureResult],
                               postProcess: ArrayBuffer[ArchitectureResult],
                               rankedPostProcess: ArrayBuffer[ArchitectureResult],
                             ): Unit = {

    log(s"\t\t${preProcess.size} architectures were processed")
    log(s"\t\t${postProcess.size} optimized architectures were evaluated")
    log(s"\t\t${rankedPostProcess.size} architectures meet performance criteria")

    if (preProcess.isEmpty || rankedPostProcess.isEmpty) return

    val preProcessBest = preProcess.head
    val postProcessBest = rankedPostProcess.head

    val cycleImprovement = (preProcessBest.simulationResult.cycle - postProcessBest.simulationResult.cycle).toDouble /
      preProcessBest.simulationResult.cycle * 100
    log(s"\t\tCycle time: ${String.format("%.2f", cycleImprovement)}% improvement")

    val avgSramSizeBefore = (preProcessBest.architecture.singleBufferLimitKbA +
      preProcessBest.architecture.singleBufferLimitKbB +
      preProcessBest.architecture.singleBufferLimitKbC) / 3.0
    val avgSramSizeAfter = (postProcessBest.architecture.singleBufferLimitKbA +
      postProcessBest.architecture.singleBufferLimitKbB +
      postProcessBest.architecture.singleBufferLimitKbC) / 3.0
    val sramReduction = (avgSramSizeBefore - avgSramSizeAfter) / avgSramSizeBefore * 100
    log(s"\t\tAverage SRAM size: ${String.format("%.2f", sramReduction)}% reduction")

    if (preProcessBest.architecture.streamingDimensionSize != postProcessBest.architecture.streamingDimensionSize) {
      val streamDimChange = (preProcessBest.architecture.streamingDimensionSize - postProcessBest.architecture.streamingDimensionSize).toDouble /
        preProcessBest.architecture.streamingDimensionSize * 100
      log(s"\t\tStreaming dimension: ${String.format("%.2f", streamDimChange)}% reduction")
    }

    val initialAvgUtilization = (preProcessBest.simulationResult.averageMemoryUtilizationA
      + preProcessBest.simulationResult.averageMemoryUtilizationB
      + preProcessBest.simulationResult.averageMemoryUtilizationC) / 3.0
    val finalAvgUtilization = (postProcessBest.simulationResult.averageMemoryUtilizationA
      + postProcessBest.simulationResult.averageMemoryUtilizationB
      + postProcessBest.simulationResult.averageMemoryUtilizationC) / 3.0
    val utilizationImprovement = finalAvgUtilization - initialAvgUtilization
    log(s"\t\tAverage memory utilization ${String.format("%.2f", utilizationImprovement)}% increase")

    if (preProcessBest.simulationResult.energyPj.isDefined && postProcessBest.simulationResult.energyPj.isDefined) {
      val energyImprovement = (preProcessBest.simulationResult.energyPj.get - postProcessBest.simulationResult.energyPj.get) /
        preProcessBest.simulationResult.energyPj.get * 100
      log(s"\t\tEnergy: ${String.format("%.2f", energyImprovement)}% improvement")
    }

    if (preProcessBest.simulationResult.areaUm2.isDefined && postProcessBest.simulationResult.areaUm2.isDefined) {
      val areaImprovement = (preProcessBest.simulationResult.areaUm2.get - postProcessBest.simulationResult.areaUm2.get) /
        preProcessBest.simulationResult.areaUm2.get * 100
      log(s"\t\tArea: ${String.format("%.2f", areaImprovement)}% improvement")
    }

    if (preProcessBest.simulationResult.tops.isDefined && postProcessBest.simulationResult.tops.isDefined) {
      val oldValue = preProcessBest.simulationResult.tops.get
      val newValue = postProcessBest.simulationResult.tops.get
      if (oldValue > 0 && newValue > 0) {
        val improvement = (newValue - oldValue) / oldValue * 100
        log(s"\t\t\tTOPS/W/mm² Efficiency: ${String.format("%.2f", improvement)}% improvement")
      }
    }
  }

  private def logSummary(ArchitectureResult: ArchitectureResult): Unit = {
    val simulationResult = ArchitectureResult.simulationResult
    val architecture = ArchitectureResult.architecture

    if(simulationResult.isEnergyReportValid && simulationResult.isAreaReportValid){
      log(s"\t[${architecture.arrayConfig.arrayConfigString}]")
      log(s"\t\tArray Active Cycle: ${simulationResult.arrayActiveCount}")
      log(s"\t\tCycle: ${simulationResult.cycle}")
      log(s"\t\tArea: ${String.format("%.2f", simulationResult.areaUm2.get)} um²")
      log(s"\t\tEnergy: ${String.format("%.2f", simulationResult.energyPj.get)} pJ")
      log(s"\t\tTOPS/W/mm^2: ${String.format("%.2f", simulationResult.tops.get)}")
      log(s"\t\tStreaming Dimension Size: ${architecture.streamingDimensionSize}")
      log(s"\t\tSingleBuffer A: ${architecture.singleBufferLimitKbA} KB")
      log(s"\t\tSingleBuffer B: ${architecture.singleBufferLimitKbB} KB")
      log(s"\t\tSingleBuffer C: ${architecture.singleBufferLimitKbC} KB")
      log(s"\t\tAverage Memory Utilization: ${String.format("%.2f", simulationResult.averageMemoryUtilization)} %\n")
      log(s"\t\t[Array Bandwidth Information]")
      log(s"\t\t\tInput Bandwidth A: ${architecture.arrayConfig.bandwidthOfInputA}")
      log(s"\t\t\tInput Bandwidth B: ${architecture.arrayConfig.bandwidthOfInputB}")
      log(s"\t\t\tOutput Bandwidth C: ${architecture.arrayConfig.outputBandwidth}\n")
      log(s"\t\t[SRAM Modeling Information]")
      log(s"\t\t\t[Input SRAM A]")
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramA.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramA.bandwidthBits}")
      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramA.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramA.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramA.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramA.areaUm2} um^2\n")
      log(s"")
      log(s"\t\t\t[Weight SRAM B]")
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramB.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramB.bandwidthBits}")
      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramB.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramB.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramB.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramB.areaUm2} um^2\n")
      log(s"")
      log(s"\t\t\t[Output SRAM C]")
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramC.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramC.bandwidthBits}")
      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramC.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramC.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramC.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramC.areaUm2} um^2\n")
    } else {
      log(s"\t[${architecture.arrayConfig.arrayConfigString}]")
      log(s"\t\tArray Active Cycle: ${simulationResult.arrayActiveCount}")
      log(s"\t\tCycle: ${simulationResult.cycle},")
      log(s"\t\tStreaming Dimension Size: ${architecture.streamingDimensionSize}")
      log(s"\t\tSingleBuffer A: ${architecture.singleBufferLimitKbA} KB")
      log(s"\t\tSingleBuffer B: ${architecture.singleBufferLimitKbB} KB")
      log(s"\t\tSingleBuffer C: ${architecture.singleBufferLimitKbC} KB")
      log(s"\t\tAverage Memory Utilization: ${String.format("%.2f", simulationResult.averageMemoryUtilization)} %\n")
      log(s"\t\t[Array Bandwidth Information]")
      log(s"\t\t\tInput Bandwidth A: ${architecture.arrayConfig.bandwidthOfInputA}")
      log(s"\t\t\tInput Bandwidth B: ${architecture.arrayConfig.bandwidthOfInputB}")
      log(s"\t\t\tOutput Bandwidth C: ${architecture.arrayConfig.outputBandwidth}\n")
      log(s"\t\t[SRAM Modeling information]")
      log(s"\t\t\t[Input SRAM A]")
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramA.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramA.bandwidthBits}")
      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramA.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramA.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramA.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramA.areaUm2} um^2\n")
      log(s"")
      log(s"\t\t\t[Weight SRAM B]")
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramB.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramB.bandwidthBits}")
      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramB.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramB.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramB.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramB.areaUm2} um^2\n")
      log(s"")
      log(s"\t\t\t[Output SRAM C]")
      log(s"\t\t\t\tBank Capacity: ${simulationResult.sramModelDataTable.get.sramC.capacityKb} KB")
      log(s"\t\t\t\tBank Bandwidth: ${simulationResult.sramModelDataTable.get.sramC.bandwidthBits}")
      log(s"\t\t\t\tBank Read Energy Per Access: ${simulationResult.sramModelDataTable.get.sramC.readEnergyPj} pJ")
      log(s"\t\t\t\tBank Write Energy Per Access: ${simulationResult.sramModelDataTable.get.sramC.writeEnergyPj} pJ")
      log(s"\t\t\t\tBank Leakage Power: ${simulationResult.sramModelDataTable.get.sramC.leakagePowerPw} pW")
      log(s"\t\t\t\tBank Area: ${simulationResult.sramModelDataTable.get.sramC.areaUm2} um^2\n")
    }
  }

  private def logCsv(ArchitectureResult: ArchitectureResult): Unit = {
    val simulationResult = ArchitectureResult.simulationResult
    val architecture = ArchitectureResult.architecture

    if(simulationResult.isEnergyReportValid && simulationResult.isAreaReportValid) {
      log(s"\t${architecture.arrayConfig.dataflow}, " +
        s"${architecture.arrayConfig.asArrayDimension.arrayDimensionString}, " +
        s"${simulationResult.cycle}, " +
        s"${String.format("%.2f", simulationResult.areaUm2.get)}, " +
        s"${String.format("%.2f", simulationResult.energyPj.get)}, " +
        s"${String.format("%.6f", simulationResult.tops.get)}, " +
        s"${architecture.streamingDimensionSize}, " +
        s"${architecture.singleBufferLimitKbA}, " +
        s"${architecture.singleBufferLimitKbB}, " +
        s"${architecture.singleBufferLimitKbC}, " +
        s"${String.format("%.2f", simulationResult.averageMemoryUtilization)}")
    } else {
      log(s"\t${architecture.arrayConfig.dataflow}, " +
        s"${architecture.arrayConfig.asArrayDimension.arrayDimensionString}, " +
        s"${simulationResult.arrayActiveCount}, " +
        s"${simulationResult.cycle}, " +
        s"${String.format("%.2f", simulationResult.areaUm2.get)}, " +
        s"${String.format("%.2f", simulationResult.energyPj.get)}, " +
        s"${String.format("%.6f", simulationResult.tops.get)}, " +
        s"${architecture.streamingDimensionSize}, " +
        s"${architecture.singleBufferLimitKbA}, " +
        s"${architecture.singleBufferLimitKbB}, " +
        s"${architecture.singleBufferLimitKbC}," +
        s"${String.format("%.2f", simulationResult.averageMemoryUtilization)}")
    }
  }

  private def logEasyCsv(ArchitectureResult: ArchitectureResult): Unit = {
    val simulationResult = ArchitectureResult.simulationResult
    val architecture = ArchitectureResult.architecture

    if(simulationResult.isEnergyReportValid && simulationResult.isAreaReportValid) {
      log(s"\t${architecture.arrayConfig.dataflow} " +
        s"${architecture.arrayConfig.asArrayDimension.arrayDimensionString}")
    }
  }

  private def logOptimizationSummary(): Unit = {
    log("\n[Optimization Summary]")

    log("\t[Optimization Process Summary]")
    log(s"\t\tStarting architectures: ${architectureCandidates.size}")
    log(s"\t\tExecutable architectures: ${executableCandidates.size}")
    log(s"\t\t[Process 1] Simulated architectures: ${rankedCalculatedResults.size}, Margin percent: $processOneMargin %")
    log(s"\t\t[Process 2] RL optimized architectures: ${archOptimizedResults.size}, Margin percent: $processTwoMargin %")
    log(s"\t\t[Process 3] Single SRAM optimized architectures: ${rankedSingleSramOptimizedResults.size}, Margin percent: $processThreeMargin %")
    log(s"\t\tFinal ranked architectures: ${rankedSingleSramOptimizedResults.size}")

    if (rankedCalculatedResults.isEmpty || rankedSingleSramOptimizedResults.isEmpty) {
      log("\n\t[Overall Optimization Improvement]")
      log("\t\tInsufficient results to compute improvement statistics")
      log("")
      return
    }

    log("\n\t[Overall Optimization Improvement]")

    val initialBest = rankedCalculatedResults.minBy(_.simulationResult.cycle)
    val finalBest = rankedSingleSramOptimizedResults.head

    val cycleImprovement = (initialBest.simulationResult.cycle - finalBest.simulationResult.cycle).toDouble /
      initialBest.simulationResult.cycle * 100
    log(s"\t\tCycle time: ${String.format("%.2f", cycleImprovement)} % improvement")

    val avgSramSizeBefore = (initialBest.architecture.singleBufferLimitKbA +
      initialBest.architecture.singleBufferLimitKbB +
      initialBest.architecture.singleBufferLimitKbC) / 3.0
    val avgSramSizeAfter = (finalBest.architecture.singleBufferLimitKbA +
      finalBest.architecture.singleBufferLimitKbB +
      finalBest.architecture.singleBufferLimitKbC) / 3.0
    val sramReduction = (avgSramSizeBefore - avgSramSizeAfter) / avgSramSizeBefore * 100
    log(s"\t\tAverage SRAM size: ${String.format("%.2f", sramReduction)} % reduction")

    if (initialBest.architecture.streamingDimensionSize != finalBest.architecture.streamingDimensionSize) {
      val streamDimChange = (initialBest.architecture.streamingDimensionSize - finalBest.architecture.streamingDimensionSize).toDouble /
        initialBest.architecture.streamingDimensionSize * 100
      log(s"\t\tHalving streaming dimension happened streaming dimension: ${String.format("%.2f", streamDimChange)} % reduction")
    }

    val initialAvgUtilization = initialBest.simulationResult.averageMemoryUtilization
    val finalAvgUtilization = finalBest.simulationResult.averageMemoryUtilization
    val utilizationImprovement = finalAvgUtilization - initialAvgUtilization
    log(s"\t\tAverage memory utilization ${String.format("%.2f", utilizationImprovement)} % increase")

    if (initialBest.simulationResult.energyPj.isDefined && finalBest.simulationResult.energyPj.isDefined) {
      val energyImprovement = (initialBest.simulationResult.energyPj.get - finalBest.simulationResult.energyPj.get) /
        initialBest.simulationResult.energyPj.get * 100
      log(s"\t\tEnergy: ${String.format("%.2f", energyImprovement)} % improvement")
    }

    if (initialBest.simulationResult.areaUm2.isDefined && finalBest.simulationResult.areaUm2.isDefined) {
      val areaImprovement = (initialBest.simulationResult.areaUm2.get - finalBest.simulationResult.areaUm2.get) /
        initialBest.simulationResult.areaUm2.get * 100
      log(s"\t\tArea: ${String.format("%.2f", areaImprovement)} % improvement")
    }

    log("")
  }

  // ============================================================
  // Shared infrastructure methods (same as original)
  // ============================================================

  private def checkInitialStreamingDimensions(archBuffer: ArrayBuffer[Architecture]): ArrayBuffer[Architecture] = {
    val executableArchBuffer = ArrayBuffer.empty[Architecture]

    archBuffer.foreach { arch =>
      var currentArch = arch
      var success = false
      var attempts = 0
      val maxAttempts = 10

      while (!success && attempts < maxAttempts) {
        val layer = new Layer(
          layerName = simConfig.layerName,
          gemmDimension = simConfig.layerGemmDimension,
          arrayConfig = currentArch.arrayConfig,
          streamingDimensionSize = currentArch.streamingDimensionSize,
          offChipMemoryUploadOrder = currentArch.offChipMemoryUploadOrder,
          loggerOption = loggerOption
        )

        val tileSizeA = layer.operationVector.head.generateTileA.dims.memorySize
        val tileSizeB = layer.operationVector.head.generateTileB.dims.memorySize
        val tileSizeC = layer.operationVector.head.generateTileC.dims.memorySize

        buildSrams(
          arrayConfig = currentArch.arrayConfig,
          simConfig = simConfig,
          singleBufferLimitKbA = currentArch.singleBufferLimitKbA,
          singleBufferLimitKbB = currentArch.singleBufferLimitKbB,
          singleBufferLimitKbC = currentArch.singleBufferLimitKbC,
          tileSizeA = tileSizeA,
          tileSizeB = tileSizeB,
          tileSizeC = tileSizeC,
          loggerOption = loggerOption,
        ) match {
          case Right(_) =>
            success = true
            executableArchBuffer += currentArch
          case Left(_) =>
            if (currentArch.streamingDimensionSize > 1) {
              val newStreamingDimSize = Math.max(1, currentArch.streamingDimensionSize / 2)
              currentArch = currentArch.withStreamingDimensionSize(newStreamingDimSize)
              attempts += 1
            } else {
              attempts = maxAttempts
            }
        }
      }

      if (!success) {
        log(s"\t\tFailed to build arch ${arch.arrayConfig.arrayConfigString}")
        log(s"\t\tInitial streaming dimension is too high change max attempts current maximum attempts is $maxAttempts")
        log("")
      }
    }

    executableArchBuffer
  }

  private def evaluateAllArchitectures(architectureBuffer: ArrayBuffer[Architecture]): ArrayBuffer[ArchitectureResult] = {
    val parallelResults = architectureBuffer.par.map { arch =>
      buildAndRunSimulation(architecture = arch)
    }
    val resultBuffer = new ArrayBuffer[ArchitectureResult]()
    resultBuffer ++= parallelResults.seq
    resultBuffer
  }

  private def halfSingleSramSizes(
                                   archResultBuffer: ArrayBuffer[ArchitectureResult],
                                 ): ArrayBuffer[ArchitectureResult] = {

    @tailrec
    def halfSingleSramSize(currentEval: ArchitectureResult): ArchitectureResult = {
      val currentArch = currentEval.architecture
      val currentSingleSramSize = currentArch.arrayConfig.dataflow match {
        case Dataflow.Is => currentArch.singleBufferLimitKbA
        case Dataflow.Os => currentArch.singleBufferLimitKbA
        case Dataflow.Ws => currentArch.singleBufferLimitKbB
      }

      if (currentArch.arrayConfig.dataflow == Dataflow.Os) {
        assert(currentArch.singleBufferLimitKbA == currentArch.singleBufferLimitKbB,
          s"For Output Stationary dataflow, SRAM A and SRAM B must have the same size. " +
            s"Found: SRAM A = ${currentArch.singleBufferLimitKbA} KB, SRAM B = ${currentArch.singleBufferLimitKbB} KB")
      }

      val currentSimulationResult = currentEval.simulationResult

      if (currentSingleSramSize <= minSramSize) return currentEval

      val nextSramSize = currentSingleSramSize / 2
      val nextArchitecture = currentArch.arrayConfig.dataflow match {
        case Dataflow.Is => currentArch.withSramBufferSize(nextSramSize, DataType.A)
        case Dataflow.Os =>
          currentArch
            .withSramBufferSize(nextSramSize, DataType.A)
            .withSramBufferSize(nextSramSize, DataType.B)
        case Dataflow.Ws => currentArch.withSramBufferSize(nextSramSize, DataType.B)
      }

      val newEval = buildAndRunSimulation(nextArchitecture)

      if (isImproved(currentResult = currentSimulationResult, newResult = newEval.simulationResult)) {
        halfSingleSramSize(newEval)
      } else {
        currentEval
      }
    }

    val parallelResults = archResultBuffer.par.map(halfSingleSramSize)
    val resultBuffer = ArrayBuffer.empty[ArchitectureResult]
    resultBuffer ++= parallelResults.seq
    resultBuffer
  }

  private def rankResults(
                           results: ArrayBuffer[ArchitectureResult],
                           marginPercent: Double,
                         ): ArrayBuffer[ArchitectureResult] = {
    if (results.isEmpty) {
      println(s"Warning: No results to rank - returning empty buffer")
      log(s"\t\tWarning: No results to rank - returning empty buffer")
      return ArrayBuffer.empty[ArchitectureResult]
    }
    val metric = simConfig.metric

    val threshold = metric match {
      case SystemArchitectureOptimizerRl.OptimizationMetric.Cycle =>
        val minCycle: Long = results.map(_.simulationResult.cycle).min
        minCycle.toDouble * (1.0 + marginPercent / 100.0)
      case SystemArchitectureOptimizerRl.OptimizationMetric.Energy =>
        val minEnergy = results.flatMap(_.simulationResult.energyPj).min
        minEnergy * (1.0 + marginPercent / 100.0)
      case SystemArchitectureOptimizerRl.OptimizationMetric.Area =>
        val minArea = results.flatMap(_.simulationResult.areaUm2).min
        minArea * (1.0 + marginPercent / 100.0)
      case SystemArchitectureOptimizerRl.OptimizationMetric.TOPS =>
        val minAreaEnergyProduct = results.flatMap(_.simulationResult.tops).max
        minAreaEnergyProduct * (1.0 - marginPercent / 100.0)
    }

    val filteredResults = results.filter { result =>
      metric match {
        case SystemArchitectureOptimizerRl.OptimizationMetric.Cycle =>
          result.simulationResult.cycle <= threshold.toLong
        case SystemArchitectureOptimizerRl.OptimizationMetric.Energy =>
          result.simulationResult.energyPj.exists(_ <= threshold)
        case SystemArchitectureOptimizerRl.OptimizationMetric.Area =>
          result.simulationResult.areaUm2.exists(_ <= threshold)
        case SystemArchitectureOptimizerRl.OptimizationMetric.TOPS =>
          result.simulationResult.tops.exists(_ >= threshold)
      }
    }

    val sortedResults = metric match {
      case SystemArchitectureOptimizerRl.OptimizationMetric.Cycle =>
        filteredResults.sortBy(_.simulationResult.cycle)
      case SystemArchitectureOptimizerRl.OptimizationMetric.Energy =>
        filteredResults.sortBy(_.simulationResult.energyPj.get)
      case SystemArchitectureOptimizerRl.OptimizationMetric.Area =>
        filteredResults.sortBy(_.simulationResult.areaUm2.get)
      case SystemArchitectureOptimizerRl.OptimizationMetric.TOPS =>
        filteredResults.sortBy(_.simulationResult.tops.get).reverse
    }

    sortedResults
  }

  private def buildAndRunSimulation(architecture: Architecture): ArchitectureResult = {
    buildSimulationComp(architecture) match {
      case Right(comp) =>
        val result = runSimulation(comp)
        ArchitectureResult(architecture = architecture, simulationResult = result)
      case Left(_) =>
        ArchitectureResult(architecture = architecture, simulationResult = SimulationResult(Long.MaxValue, Double.MaxValue, Double.MaxValue))
    }
  }

  private def buildSrams(
                          arrayConfig: ArrayConfig,
                          simConfig: SystemArchitectureOptimizerRl.SimulationConfig,
                          singleBufferLimitKbA: Int,
                          singleBufferLimitKbB: Int,
                          singleBufferLimitKbC: Int,
                          tileSizeA: Int,
                          tileSizeB: Int,
                          tileSizeC: Int,
                          loggerOption: LoggerOption
                        ): Either[SramBuildError, (DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram)] = {

    val capacityA = singleBufferLimitKbA * 8 * 1024 / tileSizeA
    val capacityB = singleBufferLimitKbB * 8 * 1024 / tileSizeB
    val capacityC = singleBufferLimitKbC * 8 * 1024 / tileSizeC

    val sramReferenceDataA: Option[SramReferenceData] = simConfig.sramReferenceDataVector.flatMap { vector =>
      vector.find { data =>
        data.capacityKb == singleBufferLimitKbA && data.bandwidthBits >= arrayConfig.bandwidthOfInputA
      }
    }
    val sramReferenceDataB: Option[SramReferenceData] = simConfig.sramReferenceDataVector.flatMap { vector =>
      vector.find { data =>
        data.capacityKb == singleBufferLimitKbB && data.bandwidthBits >= arrayConfig.bandwidthOfInputB
      }
    }
    val sramReferenceDataC: Option[SramReferenceData] = simConfig.sramReferenceDataVector.flatMap { vector =>
      vector.find { data =>
        data.capacityKb == singleBufferLimitKbC && data.bandwidthBits >= arrayConfig.outputBandwidth
      }
    }

    if (!(capacityA > 0 && capacityB > 0 && capacityC > 0)) {
      Left(SramBuildError("SRAM Cannot contain even 1 tile"))
    } else if (sramReferenceDataA.isEmpty || sramReferenceDataB.isEmpty || sramReferenceDataC.isEmpty) {
      Left(SramBuildError("Cannot find SRAM data from external reports"))
    } else {
      val sramA = new DoubleBufferSram(
        dataType = DataType.A,
        outputBandwidth = arrayConfig.bandwidthOfInputA,
        singleBufferTileCapacity = capacityA,
        singleBufferLimitKb = singleBufferLimitKbA,
        referenceData = sramReferenceDataA,
        loggerOption = loggerOption
      )
      val sramB = new DoubleBufferSram(
        dataType = DataType.B,
        outputBandwidth = arrayConfig.bandwidthOfInputB,
        singleBufferTileCapacity = capacityB,
        singleBufferLimitKb = singleBufferLimitKbB,
        referenceData = sramReferenceDataB,
        loggerOption = loggerOption
      )
      val sramC = new OutputDoubleBufferSram(
        outputBandwidth = simConfig.offChipMemoryBandwidth,
        singleBufferTileCapacity = capacityC,
        singleBufferLimitKb = singleBufferLimitKbC,
        referenceData = sramReferenceDataC,
        loggerOption = loggerOption
      )
      Right((sramA, sramB, sramC))
    }
  }

  private def buildSimulationComp(
                                   architecture: Architecture
                                 ): Either[SramBuildError, (Layer, OffChipMemory, Array, (DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram), Interface)] = {

    val layer = new Layer(
      layerName = simConfig.layerName,
      gemmDimension = simConfig.layerGemmDimension,
      arrayConfig = architecture.arrayConfig,
      streamingDimensionSize = architecture.streamingDimensionSize,
      offChipMemoryUploadOrder = architecture.offChipMemoryUploadOrder,
      loggerOption = loggerOption
    )
    val offChipMemory = new OffChipMemory(
      outputBandwidth = simConfig.offChipMemoryBandwidth,
      referenceData = simConfig.offChipMemoryReferenceData,
      loggerOption = loggerOption
    )
    val array = new Array(
      arrayConfig = architecture.arrayConfig,
      loggerOption = loggerOption
    )

    buildSrams(
      arrayConfig = architecture.arrayConfig,
      simConfig = simConfig,
      singleBufferLimitKbA = architecture.singleBufferLimitKbA,
      singleBufferLimitKbB = architecture.singleBufferLimitKbB,
      singleBufferLimitKbC = architecture.singleBufferLimitKbC,
      tileSizeA = layer.operationVector.head.generateTileA.dims.memorySize,
      tileSizeB = layer.operationVector.head.generateTileB.dims.memorySize,
      tileSizeC = layer.operationVector.head.generateTileC.dims.memorySize,
      loggerOption = loggerOption
    ) match {
      case Right(srams@(sramA, sramB, sramC)) =>
        val interface = new Interface(
          offChipMemory = offChipMemory,
          sramA = sramA,
          sramB = sramB,
          sramC = sramC,
          array = array
        )
        Right((layer, offChipMemory, array, srams, interface))
      case Left(error) =>
        Left(error)
    }
  }

  private def runSimulation(
                             components: (Layer, OffChipMemory, Array, (DoubleBufferSram, DoubleBufferSram, OutputDoubleBufferSram), Interface)
                           ): SimulationResult = {

    val (layer, offChipMemory, array, srams, interface) = components
    val (sramA, sramB, sramC) = srams

    try {
      val simulation = new SystemSimulator(
        offChipMemory = offChipMemory,
        sramA = sramA,
        sramB = sramB,
        sramC = sramC,
        interface = interface,
        layer = layer,
        array = array,
        loggerOption = loggerOption,
      )

      simulation.startSimulation()

      SimulationResult(
        totalOperationNumber = simulation.getTotalOperationNumber,
        tileSizeA = simulation.getTileSizeA,
        tileSizeB = simulation.getTileSizeB,
        tileSizeC = simulation.getTileSizeC,
        skipTileCountA = simulation.getSkipTileCountA,
        skipTileCountB = simulation.getSkipTileCountB,
        singleBufferTileCapacityA = simulation.getSingleBufferTileCapacityA,
        singleBufferTileCapacityB = simulation.getSingleBufferTileCapacityB,
        singleBufferTileCapacityC = simulation.getSingleBufferTileCapacityC,
        arrayInputBandwidthA = simulation.getArrayInputBandwidthA,
        arrayInputBandwidthB = simulation.getArrayInputBandwidthB,
        arrayOutputBandwidthC = simulation.getArrayOutputBandwidthC,
        arrayCapacityA = simulation.getArrayCapacityA,
        arrayCapacityB = simulation.getArrayCapacityB,
        cycle = simulation.getTotalCycle,
        arrayActiveCount = simulation.getArrayActiveCount,
        offChipMemoryReadAccessCount = simulation.getOffChipMemoryReadAccessCount,
        offChipMemoryWriteAccessCount = simulation.getOffChipMemoryWriteAccessCount,
        sramReadAccessCountA = simulation.getSramReadAccessCountA,
        sramWriteAccessCountA = simulation.getSramWriteAccessCountA,
        sramReadAccessCountB = simulation.getSramReadAccessCountB,
        sramWriteAccessCountB = simulation.getSramWriteAccessCountB,
        sramReadAccessCountC = simulation.getSramReadAccessCountC,
        sramWriteAccessCountC = simulation.getSramWriteAccessCountC,
        offChipMemoryHitRatio = simulation.getTotalOffChipMemoryHitCount,
        offChipMemoryMissRatio = simulation.getTotalOffChipMemoryMissCount,
        sramHitRatioA = simulation.getSramHitRatioA,
        sramHitRatioB = simulation.getSramHitRatioB,
        sramMissRatioA = simulation.getSramMissRatioA,
        sramMissRatioB = simulation.getSramMissRatioB,
        sramHitRatio = simulation.getTotalSramHitRatio,
        sramMissRatio = simulation.getTotalSramMissRatio,
        offChipMemoryStallCount = simulation.getOffChipMemoryStallCount,
        firstFillUpCycleA = simulation.getFirstFillUptCycleA,
        bufferSwapCountA = simulation.getBufferSwapCountA,
        bufferSwapStallCountA = simulation.getBufferSwapStallCountA,
        firstFillUpCycleB = simulation.getFirstFillUptCycleB,
        bufferSwapCountB = simulation.getBufferSwapCountB,
        bufferSwapStallCountB = simulation.getBufferSwapStallCountB,
        firstFillUpCycleC = simulation.getFirstFillUptCycleC,
        bufferSwapCountC = simulation.getBufferSwapCountC,
        bufferSwapStallCountC = simulation.getBufferSwapStallCountC,
        averageMemoryUsageKbA = simulation.getAverageMemoryUsageKbA,
        averageMemoryUtilizationA = simulation.getAverageMemoryUtilizationA,
        averageMemoryUsageKbB = simulation.getAverageMemoryUsageKbB,
        averageMemoryUtilizationB = simulation.getAverageMemoryUtilizationB,
        averageMemoryUsageKbC = simulation.getAverageMemoryUsageKbC,
        averageMemoryUtilizationC = simulation.getAverageMemoryUtilizationC,
        averageMemoryUtilization = simulation.getAverageMemoryUtilization,
        offChipMemoryReferenceData = simulation.getOffChipMemoryRefData,
        sramModelDataTable = simulation.getSramModelDataTable,
        arraySynthesisSource = simulation.getArraySynthesisSource,
        arraySynthesisData = simulation.getArraySynthesisData,
        sramReadEnergyPjA = simulation.getSramReadEnergyA,
        sramWriteEnergyPjA = simulation.getSramWriteEnergyA,
        sramLeakageEnergyPjA = simulation.getSramLeakageEnergyA,
        sramEnergyPjA = simulation.getSramEnergyA,
        sramReadEnergyPjB = simulation.getSramReadEnergyB,
        sramWriteEnergyPjB = simulation.getSramWriteEnergyB,
        sramLeakageEnergyPjB = simulation.getSramLeakageEnergyB,
        sramEnergyPjB = simulation.getSramEnergyB,
        sramReadEnergyPjC = simulation.getSramReadEnergyC,
        sramWriteEnergyPjC = simulation.getSramWriteEnergyC,
        sramLeakageEnergyPjC = simulation.getSramLeakageEnergyC,
        sramEnergyPjC = simulation.getSramEnergyC,
        offChipMemoryReadEnergyPj = simulation.getOffChipMemoryReadEnergy,
        offChipMemoryWriteEnergyPj = simulation.getOffChipMemoryWriteEnergy,
        offChipMemoryEnergyPj = simulation.getOffChipMemoryEnergy,
        arrayDynamicEnergyPj = simulation.getArrayDynamicEnergy,
        arrayLeakageEnergyPj = simulation.getArrayLeakageEnergy,
        arrayEnergy = simulation.getArrayEnergy,
        energyPj = simulation.getTotalEnergy,
        sramAreaUm2A = simulation.getSramAreaA,
        sramAreaUm2B = simulation.getSramAreaB,
        sramAreaUm2C = simulation.getSramAreaC,
        arrayAreaUm2 = simulation.getArrayArea,
        areaUm2 = simulation.getArea,
        tops = simulation.calculateTOPS
      )
    } catch {
      case _: RunTimeError =>
        Console.err.println("Program Wrong")
        sys.exit(1)
      case error: SramBuildError =>
        log(s"\t\t$error")
        SimulationResult(Long.MaxValue, Double.MaxValue, Double.MaxValue)
    }
  }
}
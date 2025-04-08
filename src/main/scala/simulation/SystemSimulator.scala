package simulation

import scala.util.control.Breaks._

//TODO ordering the arguments
class SystemSimulator(
  private val dram: Dram,
  private val sramA: DoubleBufferSram,
  private val sramB: DoubleBufferSram,
  private val sramC: OutputDoubleBufferSram,
  private val interface: Interface,
  private val layer: Layer,
  private val array: Array,
  private val debugStartCycle: Long = 0,
  private val debugEndCycle: Long = 0,
  private val debugMode: Boolean = false,
  private val loggerOption: LoggerOption
) extends Logger with PowerAreaCalculator {

  assert(areAllHardwareQueueEmpty, "[error] Hardware is not empty")
  setMode(loggerOption)

  private val clockPeriod: Double = 2e-9
  private var cycle: Long = -1
  private var lastProgressPercent: Int = -1

  def getDramRefData: Option[DramReferenceData] = dram.referenceData


  def getSramRefDataTable: Option[SramDataTable] =
    for {
      sramDataA <- sramA.referenceData
      sramDataB <- sramB.referenceData
      sramDataC <- sramC.referenceData
    } yield SramDataTable(
      Some(sramDataA),
      Some(sramDataB),
      Some(sramDataC),
    )

  def getArraySynthesisData: Option[ArraySynthesisData] = array.arrayConfig.arraySynthesisData
  def getArraySynthesisSource: Option[ArraySynthesisSource.Value] = array.arrayConfig.arraySynthesisSource


  def startSimulation(): Unit = {
    cycle = runSimulationLoop()
  }

  private def runSimulationLoop(): Long = {

    var cycle: Long = 0

    require(layer.operationVector.nonEmpty, "Empty operation vector function is called in wrong place")
    dram.initDram(layer.operationVector, array.arrayConfig.dataflow)
    array.uploadOperationVector(layer.operationVector)
    sramA.initTileSchedule(layer.operationVector)
    sramB.initTileSchedule(layer.operationVector)

    breakable {
      while (!areAllHardwareQueueEmpty) {
        interface.updateCycle(cycle)

//        val currentProgress = (array.schedule.count(_.isCalculated) * 100) / layer.operationVector.size
//        if (currentProgress > lastProgressPercent) {
//          println(s"Progress: $currentProgress %")
//          println(s"Cycle: $cycle")
//          lastProgressPercent = currentProgress
//        }

        if (debugMode)
          if (debugStartCycle <= cycle && cycle < debugEndCycle) {
            println("Program stop for debugging purpose")
            printCompilationState(cycle)
          } else if (debugEndCycle == cycle)
            break()

        dram.update(interface)
        sramA.update(interface)
        sramB.update(interface)
        array.update(interface)
        sramC.update(interface)

        interface.checkTraffic() match {
          case Right(_) =>
          case Left(e) =>
            println(s"Failed: ${e.getMessage}")
            printCompilationState(cycle)
            break()

        }

        if (array.capacityLeftTileA < 0) {
          Console.err.println("Capacity is a negative value")
          printCompilationState(cycle)
          break()
        }

        if (array.capacityLeftTileB() < 0) {
          Console.err.println("Capacity is a negative value")
          printCompilationState(cycle)
          break()
        }

        cycle += 1

      }
    }

    cycle

  }

  //TODO DELETE!!!! Useless METRIC!!!!
  //1. Initial Workload Metrics
  def getTotalOperationNumber: Int = layer.operationVector.size

  def getTileSizeA: Int = layer.operationVector.head.generateTileA.dims.memorySize
  def getTileSizeB: Int = layer.operationVector.head.generateTileB.dims.memorySize
  def getTileSizeC: Int = layer.operationVector.head.generateTileC.dims.memorySize

  def getSingleBufferTileCapacityA: Int = sramA.singleBufferTileCapacity
  def getSingleBufferTileCapacityB: Int = sramB.singleBufferTileCapacity
  def getSingleBufferTileCapacityC: Int = sramC.singleBufferTileCapacity
  def getSkipTileCountA: Int = dram.skipTileCountA
  def getSkipTileCountB: Int = dram.skipTileCountB

  //2. Bandwidth info
  def getArrayInputBandwidthA: Int = array.arrayConfig.bandwidthOfInputA
  def getArrayInputBandwidthB: Int = array.arrayConfig.bandwidthOfInputB
  def getArrayOutputBandwidthC: Int = array.arrayConfig.outputBandwidth
  def getArrayCapacityA: Int = array.arrayConfig.capacityOfTileA
  def getArrayCapacityB: Int = array.arrayConfig.capacityOfTileB

  //3. Performance Metrics
  def getTotalCycle: Long = cycle
  def getArrayActiveCount: Int = array.getArrayActiveCount

  //Pipeline state
  def getDramStallCount: Int = dram.dramStall
  def getFirstFillUptCycleA: Long = sramA.getFirstFillUpCycle
  def getFirstFillUptCycleB: Long = sramB.getFirstFillUpCycle
  def getFirstFillUptCycleC: Long = sramC.getFirstFillUpCycle
  def getBufferSwapCountA: Int = sramA.getBufferSwapCount
  def getBufferSwapCountB: Int = sramB.getBufferSwapCount
  def getBufferSwapCountC: Int = sramC.getBufferSwapCount
  def getBufferSwapStallCountA: Int = sramA.getBufferSwapStallCount
  def getBufferSwapStallCountB: Int = sramB.getBufferSwapStallCount
  def getBufferSwapStallCountC: Int = sramC.getBufferSwapStallCount

  //Read write log
  def getDramReadAccessCount: Long = dram.getReadAccessCount
  def getDramWriteAccessCount: Long = dram.getWriteAccessCount
  def getSramReadAccessCountA: Long = sramA.getReadAccessCount
  def getSramWriteAccessCountA: Long = sramA.getWriteAccessCount
  def getSramReadAccessCountB: Long = sramB.getReadAccessCount
  def getSramWriteAccessCountB: Long = sramB.getWriteAccessCount

  //DRAM hit miss ratio
  def getTotalDramHitCount: Double = (sramA.getDramHitCount + sramB.getDramHitCount) /
    (sramA.getDramAccessCount + sramB.getDramAccessCount)

  def getTotalDramMissCount: Double = (sramA.getDramMissCount + sramB.getDramMissCount) /
    (sramA.getDramAccessCount + sramB.getDramAccessCount)

  //SRAM hit miss ratio
  def getTotalSramHitRatio: Double = (array.getMemoryHitCountA + array.getMemoryHitCountB) /
    (array.getMemoryAccessCountA + array.getMemoryAccessCountB)

  def getTotalSramMissRatio: Double = (array.getMemoryMissCountA + array.getMemoryMissCountB) /
    (array.getMemoryAccessCountA + array.getMemoryAccessCountB)

  def getSramHitRatioA: Double = array.getMemoryHitCountA / array.getMemoryAccessCountA
  def getSramMissRatioA: Double = array.getMemoryMissCountA / array.getMemoryAccessCountA
  def getSramHitRatioB: Double = array.getMemoryHitCountB / array.getMemoryAccessCountB
  def getSramMissRatioB: Double = array.getMemoryMissCountB / array.getMemoryAccessCountB

  //4. Memory Utilization

  def getAverageMemoryUsageKbA: Double = (sramA.getAccumulatedMemoryUsage / 8.0 / 1024.0) / cycle.toDouble
  def getAverageMemoryUsageKbB: Double = (sramB.getAccumulatedMemoryUsage / 8.0 / 1024.0) / cycle.toDouble
  def getAverageMemoryUsageKbC: Double = (sramC.getAccumulatedMemoryUsage / 8.0 / 1024.0) / cycle.toDouble

  def getAverageMemoryUtilizationA: Double = sramA.getAccumulateMemoryUtilization / cycle.toDouble
  def getAverageMemoryUtilizationB: Double = sramB.getAccumulateMemoryUtilization / cycle.toDouble
  def getAverageMemoryUtilizationC: Double = sramC.getAccumulateMemoryUtilization / cycle.toDouble
  def getAverageMemoryUtilization: Double = (getAverageMemoryUtilizationA + getAverageMemoryUtilizationB + getAverageMemoryUtilizationC) / 3.0

  //5. Energy Report

  // DRAM
  def getDramReadEnergy: Option[Double] = dram.getDramReadEnergy
  def getDramWriteEnergy: Option[Double] = dram.getDramWriteEnergy
  def getDramEnergy: Option[Double] = dram.getDramEnergy

  // SRAM A
  def getSramReadEnergyA: Option[Double] = sramA.getSramReadEnergy
  def getSramWriteEnergyA: Option[Double] = sramA.getSramWriteEnergy
  def getSramLeakageEnergyA: Option[Double] = sramA.referenceData.map{ data =>
    data.leakagePowerPw * cycle * clockPeriod
  }

  def getSramEnergyA: Option[Double] = {
    for {
      read <- getSramReadEnergyA
      write <- getSramWriteEnergyA
      leakage <- getSramLeakageEnergyA
    } yield read + write + leakage
  }

  // SRAM B
  def getSramReadEnergyB: Option[Double] = sramB.getSramReadEnergy
  def getSramWriteEnergyB: Option[Double] = sramB.getSramWriteEnergy
  def getSramLeakageEnergyB: Option[Double] = sramB.referenceData.map{ data =>
    data.leakagePowerPw * cycle * clockPeriod
  }

  def getSramEnergyB: Option[Double] = {
    for {
      read <- getSramReadEnergyB
      write <- getSramWriteEnergyB
      leakage <- getSramLeakageEnergyB
    } yield read + write + leakage
  }

  // SRAM C
  def getSramReadEnergyC: Option[Double] = sramC.getSramReadEnergy
  def getSramWriteEnergyC: Option[Double] = sramC.getSramWriteEnergy
  def getSramLeakageEnergyC: Option[Double] = sramC.referenceData.map{ data =>
    data.leakagePowerPw * cycle * clockPeriod
  }

  def getSramEnergyC: Option[Double] =
    for {
      read <- getSramReadEnergyC
      write <- getSramWriteEnergyC
      leakage <- getSramLeakageEnergyC
    } yield read + write + leakage

  //ARRAY
  def getArrayDynamicEnergy: Option[Double] = array.arrayConfig.arraySynthesisData.map{ data =>
    (data.switchPowerPw + data.internalPowerPw) * array.getArrayActiveCount * clockPeriod
  }

  def getArrayLeakageEnergy: Option[Double] = array.arrayConfig.arraySynthesisData.map { data =>
    data.leakagePowerPw * cycle * clockPeriod
  }

  def getArrayEnergy: Option[Double] =
    for{
      dynamicEnergy <- getArrayDynamicEnergy
      leakageEnergy <- getArrayLeakageEnergy
    } yield dynamicEnergy + leakageEnergy

  def getTotalEnergy: Option[Double] =
    for {
      dramEnergy <- getDramEnergy
      sramEnergyA <- getSramEnergyA
      sramEnergyB <- getSramEnergyB
      arrayEnergy <- getArrayEnergy
      sramEnergyC <- getSramEnergyC


    } yield dramEnergy + sramEnergyA + sramEnergyB + sramEnergyC + arrayEnergy

  //6. Area Report
  def getSramAreaA: Option[Double] = sramA.referenceData.map(_.areaUm2)
  def getSramAreaB: Option[Double] = sramB.referenceData.map(_.areaUm2)
  def getSramAreaC: Option[Double] = sramC.referenceData.map(_.areaUm2)


  //Array
  def getArrayArea: Option[Double] = array.arrayConfig.arraySynthesisData.map { data =>
    data.areaUm2
  }

  //Total
  def getArea: Option[Double] =
    for {
      sramAreaA <- getSramAreaA
      sramAreaB <- getSramAreaB
      sramAreaC <- getSramAreaC
      arrayArea <- getArrayArea
    } yield sramAreaA + sramAreaB + sramAreaC + arrayArea

  //Util functions
  private def areAllHardwareQueueEmpty: Boolean = {
    dram.isHardwareEmpty &&
      sramA.isHardwareEmpty &&
      sramB.isHardwareEmpty &&
      sramC.isHardwareEmpty &&
      array.isHardwareEmpty
  }

  private def printCompilationState(cycle: Long): Unit = {
    log("")
    log(s"[Current Cycle: $cycle]")
    dram.printTiles()
    sramA.printTiles()
    sramB.printTiles()
    array.printTiles()
    sramC.printTiles()
    array.printSchedule()
    log("")
  }
}




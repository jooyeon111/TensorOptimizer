package simulation

import scala.util.control.Breaks._
import scala.collection.mutable

class Compiler(
  val dram: Dram,
  val sramA: DoubleBufferSram,
  val sramB: DoubleBufferSram,
  val sramC: OutputDoubleBufferSram,
  val interface: Interface,
  val layer: Layer,
  val array: Array,
              //TODO make it Option
  val sramDataReferenceVector: Vector[SramReferenceData],
  val dramReferenceDataData: DramReferenceData,
  val arrayReferenceData: ArrayReferenceData,
  val debugStartCycle: Long = 0,
  val debugEndCycle: Long = 0,
  val debugMode: Boolean = false,
  val loggerOption: LoggerOption
) extends Logger {

  assert(areAllHardwareQueueEmpty, "[error] Hardware is not empty")
  setMode(loggerOption)

  private var cycle: Long = -1

  private val clockPeriod: Double = 2e-9

  private val inputSramDataA: SramReferenceData = {
    sramDataReferenceVector
      .find{ sram =>
        (sram.capacityKb == sramA.singleBufferLimitKb) && (array.arrayConfig.bandwidthOfInputA < sram.bandwidthBits)
      }.getOrElse{
        Console.err.println("There is no SRAM info in SRAM table... reorganize SRAM output files")
        sys.exit(1)
      }
  }

  private val inputSramDataB: SramReferenceData = {
    sramDataReferenceVector
      .find{ sram =>
        (sram.capacityKb == sramB.singleBufferLimitKb) && (array.arrayConfig.bandwidthOfInputB < sram.bandwidthBits)
      }.getOrElse{
        Console.err.println("There is no SRAM info in SRAM table... reorganize SRAM output files")
        sys.exit(1)
      }
  }

  private val outputSramDataC: SramReferenceData = {
    sramDataReferenceVector
      .find{ sram =>
        (sram.capacityKb == sramC.singleBufferLimitKb) && (array.arrayConfig.outputBandwidth < sram.bandwidthBits)
      }.getOrElse{
        Console.err.println("There is no SRAM info in SRAM table... reorganize SRAM output files")
        sys.exit(1)
      }
  }

  def run(): Unit = {
    cycle = compileLayerCycle()
  }


  private def compileLayerCycle() : Long  = {

    var cycle: Long = 0

//    dram.uploadInitialTiles(layer.operationVector, array.arrayConfig.dataflow)
    require(layer.operationVector.nonEmpty, "Empty operation vector function is called in wrong place")
    dram.initDram(layer.operationVector, array.arrayConfig.dataflow)
    array.uploadOperationVector(layer.operationVector)
    array.initTileSize(layer.operationVector.head)
    sramA.initTileSchedule(layer.operationVector)
    sramA.initTileSize(layer.operationVector.head)
    sramB.initTileSchedule(layer.operationVector)
    sramB.initTileSize(layer.operationVector.head)

    breakable {
      while(!areAllHardwareQueueEmpty) {
        interface.updateCycle(cycle)

        if(debugMode)
          if( debugStartCycle <= cycle && cycle < debugEndCycle )
            printCompilationState(cycle)
          else if( debugEndCycle == cycle )
            break()

        dram.update(interface)
        sramA.update(interface)
        sramB.update(interface)
        array.update(interface)
        sramC.update(interface)

        interface.checkTraffic() match {
          case Right(_) =>

            dram.restoreTrafficState()
            sramA.restoreTrafficState()
            sramB.restoreTrafficState()
            array.restoreTrafficState()
            sramC.restoreTrafficState()
            dram.restoreTrafficState()

          case Left(e) =>

            log(s"Failed: ${e.getMessage}")
            printCompilationState(cycle)
            sys.exit(1)

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
  def getTrimTileCountA: Int = dram.trimTileCountA
  def getTrimTileCountB: Int = dram.trimTileCountB

  //2. Bandwidth info
  def getArrayInputBandwidthA: Int = array.arrayConfig.bandwidthOfInputA
  def getArrayInputBandwidthB: Int = array.arrayConfig.bandwidthOfInputB
  def getArrayOutputBandwidthC: Int = array.arrayConfig.outputBandwidth

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

  def getArrayInputStallCount: Int = array.arrayInputStallCount
  def getArrayOutputStallCount: Int = array.arrayOutputStallCount

  def getBufferSwapStallCountA: Int = sramA.getBufferSwapStallCount
  def getBufferSwapStallCountB: Int = sramB.getBufferSwapStallCount
  def getBufferSwapStallCountC: Int = sramC.getBufferSwapStallCount

  //Read write log
  def getDramLogs: mutable.Queue[DramLog] = dram.getDramLogs
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

  def getSramHitRatioA: Double =  array.getMemoryHitCountA / array.getMemoryAccessCountA
  def getSramMissRatioA: Double = array.getMemoryMissCountA / array.getMemoryAccessCountA
  def getSramHitRatioB: Double =  array.getMemoryHitCountB / array.getMemoryAccessCountB
  def getSramMissRatioB: Double = array.getMemoryMissCountB / array.getMemoryAccessCountB

  //4. Memory Utilization

  def getAverageMemoryUsageKbA: Double = (sramA.getAccumulatedMemoryUsage / 8.0 / 1024.0)/ cycle.toDouble
  def getAverageMemoryUsageKbB: Double = (sramB.getAccumulatedMemoryUsage / 8.0 / 1024.0)/ cycle.toDouble
  def getAverageMemoryUsageKbC: Double = (sramC.getAccumulatedMemoryUsage / 8.0 / 1024.0)/ cycle.toDouble

  def getAverageMemoryUtilizationA: Double = sramA.getAccumulateMemoryUtilization / cycle.toDouble
  def getAverageMemoryUtilizationB: Double = sramB.getAccumulateMemoryUtilization / cycle.toDouble
  def getAverageMemoryUtilizationC: Double = sramC.getAccumulateMemoryUtilization / cycle.toDouble

  //5. Area results
  //Final area results unit is mm^2
  def getSramAreaMmA: Double = inputSramDataA.areaUm2
  def getSramAreaMmB: Double= inputSramDataB.areaUm2
  def getSramAreaMmC: Double = outputSramDataC.areaUm2
  def getArrayAreaMm: Double = computeArrayArea
  def getDramAreaMm: Double = dramReferenceDataData.areaMm2
  def getTotalArea: Double = getSramAreaMmA + getSramAreaMmB + getSramAreaMmC + getArrayAreaMm + getDramAreaMm

  //6. Energy results
  //Final energy results unit is pJ
  //SRAM A
  def getSramReadEnergyA: Double = inputSramDataA.readEnergyPj * sramA.getReadAccessCount
  def getSramWriteEnergyA: Double = inputSramDataA.writeEnergyPj * sramA.getWriteAccessCount
  def getSramLeakageEnergyA: Double = inputSramDataA.leakagePowerPw * cycle * clockPeriod
  def getSramEnergyA: Double = getSramReadEnergyA + getSramWriteEnergyA + getSramLeakageEnergyA

  //SRAM B
  def getSramReadEnergyB: Double = inputSramDataB.readEnergyPj * sramB.getReadAccessCount
  def getSramWriteEnergyB: Double = inputSramDataB.writeEnergyPj * sramB.getWriteAccessCount
  def getSramLeakageEnergyB: Double = inputSramDataB.leakagePowerPw * cycle * clockPeriod
  def getSramEnergyB: Double = getSramReadEnergyB + getSramWriteEnergyB + getSramLeakageEnergyB

  //SRAM C
  def getSramReadEnergyC: Double = outputSramDataC.readEnergyPj * sramC.getReadAccessCount
  def getSramWriteEnergyC: Double = outputSramDataC.writeEnergyPj * sramC.getWriteAccessCount
  def getSramLeakageEnergyC: Double = outputSramDataC.leakagePowerPw * cycle * clockPeriod
  def getSramEnergyC: Double = getSramReadEnergyC + getSramWriteEnergyC + getSramLeakageEnergyC

  //DRAM
  def getDramReadEnergy: Double = dram.getReadAccessCount * dramReferenceDataData.readEnergyPj
  def getDramWriteEnergy: Double = dram.getWriteAccessCount * dramReferenceDataData.writeEnergyPj
  def getDramEnergy: Double = getDramReadEnergy + getDramWriteEnergy

  //Array
  def getArrayDynamicEnergy: Double = array.getArrayActiveCount.toDouble * computeArrayDynamicPower
  def getArrayLeakageEnergy: Double = computeArrayLeakagePower * cycle * clockPeriod
  def getArrayEnergy: Double = getArrayDynamicEnergy + getArrayLeakageEnergy

  def getTotalEnergy: Double = getSramEnergyA + getSramEnergyB + getSramEnergyC + getDramEnergy + getArrayEnergy


  private def computeArrayDynamicPower: Double = {
    array.arrayConfig.groupPeRow * arrayReferenceData.dynamicPowerGroupPeRowPj +
      array.arrayConfig.groupPeCol * arrayReferenceData.dynamicPowerGroupPeColPj +
      array.arrayConfig.vectorPeRow * arrayReferenceData.dynamicPowerVectorPeRowPj +
      array.arrayConfig.vectorPeCol * arrayReferenceData.dynamicPowerVectorPeColPj +
      array.arrayConfig.numMultiplier * arrayReferenceData.dynamicPowerNumMultiplierPj
  }

  private def computeArrayLeakagePower: Double = {
    array.arrayConfig.groupPeRow * arrayReferenceData.leakagePowerGroupPeRowPw +
      array.arrayConfig.groupPeCol * arrayReferenceData.leakagePowerGroupPeColPw +
      array.arrayConfig.vectorPeRow * arrayReferenceData.leakagePowerVectorPeRowPw +
      array.arrayConfig.vectorPeCol * arrayReferenceData.leakagePowerVectorPeColPw +
      array.arrayConfig.numMultiplier * arrayReferenceData.leakagePowerNumMultiplierPw
  }

  private def computeArrayArea: Double = {
    array.arrayConfig.groupPeRow * arrayReferenceData.areaPowerGroupPeRowUm2 +
      array.arrayConfig.groupPeCol * arrayReferenceData.areaPowerGroupPeColUm2 +
      array.arrayConfig.vectorPeRow * arrayReferenceData.areaPowerVectorPeRowUm2 +
      array.arrayConfig.vectorPeCol * arrayReferenceData.areaPowerVectorPeColUm2 +
      array.arrayConfig.numMultiplier * arrayReferenceData.areaPowerNumMultiplierUm2
  }

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
//    sramA.printSchedule()
//    sramB.printSchedule()
//    sramA.printReceivingOrder()
//    sramB.printReceivingOrder()
    log("")
  }
}



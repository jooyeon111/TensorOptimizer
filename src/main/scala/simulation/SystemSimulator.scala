package simulation

import scala.util.control.Breaks._

//TODO ordering the arguments
class SystemSimulator(
  private val offChipMemory: OffChipMemory,
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
) extends Logger {

  assert(areAllHardwareQueueEmpty, "[error] Hardware is not empty")
  setMode(loggerOption)

  private val clockPeriod: Double = 2e-9
  private var cycle: Long = -1
  private var lastProgressPercent: Int = -1

  def getOffChipMemoryRefData: Option[OffChipMemoryReferenceData] = offChipMemory.referenceData

//  private val dividedSramModelA: Option[DividedSramModel] = sliceSram(DataType.A)
//  private val dividedSramModelB: Option[DividedSramModel]  = sliceSram(DataType.B)
//  private val dividedSramModelC: Option[DividedSramModel]  = sliceSram(DataType.C)

  def getSramModelDataTable: Option[DividedSramModelTable] = {
    (sramA.referenceData, sramB.referenceData, sramC.referenceData) match {
      case (Some(sramA), Some(sramB), Some(sramC)) =>
        Some(DividedSramModelTable(sramA, sramB, sramC))
      case _ =>
        None
    }
  }

  def getArraySynthesisData: Option[ArraySynthesisData] = array.arrayConfig.arraySynthesisData
  def getArraySynthesisSource: Option[ArraySynthesisSource.Value] = array.arrayConfig.arraySynthesisSource


  def startSimulation(): Unit = {
    cycle = runSimulationLoop()
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
  def getSkipTileCountA: Int = offChipMemory.skipTileCountA
  def getSkipTileCountB: Int = offChipMemory.skipTileCountB

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
  def getOffChipMemoryStallCount: Int = offChipMemory.offChipMemoryStall
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
  def getOffChipMemoryReadAccessCount: Long = offChipMemory.getReadAccessCount
  def getOffChipMemoryWriteAccessCount: Long = offChipMemory.getWriteAccessCount
  def getSramReadAccessCountA: Long = sramA.getReadAccessCount
  def getSramWriteAccessCountA: Long = sramA.getWriteAccessCount
  def getSramReadAccessCountB: Long = sramB.getReadAccessCount
  def getSramWriteAccessCountB: Long = sramB.getWriteAccessCount

  def getSramReadAccessCountC: Long = sramC.getReadAccessCount
  def getSramWriteAccessCountC: Long = sramC.getWriteAccessCount

  def getTotalOffChipMemoryHitCount: Double = (sramA.getOffChipMemoryHitCount + sramB.getOffChipMemoryHitCount) /
    (sramA.getOffChipMemoryAccessCount + sramB.getOffChipMemoryAccessCount)

  def getTotalOffChipMemoryMissCount: Double = (sramA.getOffChipMemoryMissCount + sramB.getOffChipMemoryMissCount) /
    (sramA.getOffChipMemoryAccessCount + sramB.getOffChipMemoryAccessCount)

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

  // Off Chip Memory
  def getOffChipMemoryReadEnergy: Option[Double] = offChipMemory.getOffChipMemoryReadEnergy
  def getOffChipMemoryWriteEnergy: Option[Double] = offChipMemory.getOffChipMemoryWriteEnergy
  def getOffChipMemoryEnergy: Option[Double] = offChipMemory.getOffChipMemoryEnergy

  def getSramReadEnergyA: Option[Double] = sramA.getSramReadEnergy
  def getSramWriteEnergyA: Option[Double] = sramA.getSramWriteEnergy
  def getSramLeakageEnergyA: Option[Double] = sramA.referenceData.map{ data =>
    data.leakagePowerPw * cycle * clockPeriod
  }

//  def getSramReadEnergyA: Option[Double] = dividedSramModelA.map { sramData =>
//    sramData.referenceData.readEnergyPj * sramA.getReadAccessCount
//  }
//
//  def getSramWriteEnergyA: Option[Double] = dividedSramModelA.map { sramData =>
//    sramData.referenceData.writeEnergyPj * sramA.getWriteAccessCount * sramData.bankCount
//  }
//
//  def getSramLeakageEnergyA: Option[Double] = dividedSramModelA.map { sramData =>
//    sramData.referenceData.leakagePowerPw * cycle * clockPeriod * sramData.bankCount
//  }

  def getSramEnergyA: Option[Double] = {
    for {
      read <- getSramReadEnergyA
      write <- getSramWriteEnergyA
      leakage <- getSramLeakageEnergyA
    } yield read + write + leakage
  }

//  def getSramReadEnergyB: Option[Double] = dividedSramModelB.map { sramData =>
//    sramData.referenceData.readEnergyPj * sramB.getReadAccessCount
//  }
//
//  def getSramWriteEnergyB: Option[Double] = dividedSramModelB.map { sramData =>
//    sramData.referenceData.writeEnergyPj * sramB.getWriteAccessCount * sramData.bankCount
//  }
//
//  def getSramLeakageEnergyB: Option[Double] = dividedSramModelB.map { sramData =>
//    sramData.referenceData.leakagePowerPw * cycle * clockPeriod * sramData.bankCount
//  }

  def getSramReadEnergyB: Option[Double] = sramB.getSramReadEnergy
  def getSramWriteEnergyB: Option[Double] = sramB.getSramWriteEnergy
  def getSramLeakageEnergyB: Option[Double] = sramB.referenceData.map { data =>
    data.leakagePowerPw * cycle * clockPeriod
  }

  def getSramEnergyB: Option[Double] = {
    for {
      read <- getSramReadEnergyB
      write <- getSramWriteEnergyB
      leakage <- getSramLeakageEnergyB
    } yield read + write + leakage
  }

//  def getSramReadEnergyC: Option[Double] = dividedSramModelC.map { sramData =>
//    sramData.referenceData.readEnergyPj * sramC.getReadAccessCount * sramData.bankCount
//  }
//  def getSramWriteEnergyC: Option[Double] = dividedSramModelC.map { sramData =>
//    sramData.referenceData.writeEnergyPj * sramC.getWriteAccessCount
//  }
//  def getSramLeakageEnergyC: Option[Double] = dividedSramModelC.map { sramData =>
//    sramData.referenceData.leakagePowerPw * cycle * clockPeriod * sramData.bankCount
//  }

  def getSramReadEnergyC: Option[Double] = sramC.getSramReadEnergy
  def getSramWriteEnergyC: Option[Double] = sramC.getSramWriteEnergy
  def getSramLeakageEnergyC: Option[Double] = sramC.referenceData.map { data =>
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
      offChipMemoryEnergy <- getOffChipMemoryEnergy
      sramEnergyA <- getSramEnergyA
      sramEnergyB <- getSramEnergyB
      arrayEnergy <- getArrayEnergy
      sramEnergyC <- getSramEnergyC
    } yield offChipMemoryEnergy + sramEnergyA + sramEnergyB + sramEnergyC + arrayEnergy

  //6. Area Report
//  def getSramAreaA: Option[Double] = dividedSramModelA.map(_.referenceData.areaUm2)
//  def getSramAreaB: Option[Double] = dividedSramModelB.map(_.referenceData.areaUm2)
//  def getSramAreaC: Option[Double] = dividedSramModelC.map(_.referenceData.areaUm2)

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

  def calculateTOPS: Option[Double] =  {
//    for {
//      energy <- getTotalEnergy
//      area <- getArea
//    } yield {
//      val executionTime = cycle * clockPeriod
//      energy * area * executionTime
//    }

    for {
      energy <- getTotalEnergy // Energy in pJ
      area <- getArea // Area in μm²
    } yield {

      if (cycle <= 0 || energy <= 0 || area <= 0) {
        println(s"Warning: Invalid inputs - cycle:$cycle, energy:$energy, area:$area")
        return Some(1e-12)
      }

      val totalOps: Long = layer.gemmDimension.m.toLong * layer.gemmDimension.n.toLong * layer.gemmDimension.k.toLong

      if (totalOps <= 0) {
        println(s"Warning: No operations calculated" +
          s" M: ${layer.gemmDimension.m}" +
          s" N: ${layer.gemmDimension.n}" +
          s" K: ${layer.gemmDimension.k}" +
          s" ${totalOps}")

        return Some(1e-12)
      }

      // Use existing clockPeriod for timing
      val executionTimeSeconds = cycle * clockPeriod

      // Unit conversions
      val teraOps = totalOps / 1e12
      val powerWatts = (energy * 1e-12) / executionTimeSeconds
      val areaInMm2 = area / 1e6

      // Validate intermediate calculations
      if (executionTimeSeconds <= 0 || powerWatts <= 0 || areaInMm2 <= 0) {
        println(s"Warning: Invalid intermediate values - time:$executionTimeSeconds, power:$powerWatts, area:$areaInMm2")
        return Some(1e-12)
      }

      // Calculate TOPS/W/mm²
      val throughputTOPS: Double = teraOps / executionTimeSeconds
      val efficiency = throughputTOPS / (powerWatts * areaInMm2)

//      if (cycle < 10000) {
//        println(f"TOPS/W/mm² Debug:")
//        println(f"  Operations: ${totalOps}%.0f (${totalOps/1e9}%.3f billion)")
//        println(f"  Cycles: $cycle, Time: ${executionTimeSeconds*1e6}%.2f μs")
//        println(f"  Energy: ${energy}%.2f pJ, Power: ${powerWatts*1e6}%.2f μW")
//        println(f"  Area: ${area}%.0f μm² (${areaInMm2}%.3f mm²)")
//        println(f"  Throughput: ${throughputTOPS}%.6f TOPS")
//        println(f"  Efficiency: ${efficiency}%.6e TOPS/W/mm²")
//      }

      // Final validation
      if (efficiency.isNaN || efficiency.isInfinite || efficiency <= 0) {
        println(s"Warning: Invalid final efficiency: $efficiency")
        1e-12
      } else {
        efficiency
      }
    }
  }

  private def runSimulationLoop(): Long = {

    var cycle: Long = 0

    require(layer.operationVector.nonEmpty, "Empty operation vector function is called in wrong place")
    offChipMemory.initOffChipMemory(layer.operationVector, array.arrayConfig.dataflow)
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

        offChipMemory.update(interface)
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

        if (array.capacityLeftTileA() < 0) {
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

//  private def sliceSram(dataType: DataType.Value): Option[DividedSramModel] = {
//    if (sramReferenceDataVector.isEmpty) {
//      None
//    } else {
//      val arrayConfig = array.arrayConfig
//
//      val (bandwidth, singleBufferLimitKb, bankCountFn) = dataType match {
//        case DataType.A => (arrayConfig.bandwidthOfInputA, sramA.singleBufferLimitKb, calculateBankCount _)
//        case DataType.B => (arrayConfig.bandwidthOfInputB, sramB.singleBufferLimitKb, calculateBankCount _)
//        case DataType.C => (arrayConfig.outputBandwidth, sramC.singleBufferLimitKb, calculateBankCount _)
//      }
//
//      val bankCount = bankCountFn(bandwidth)
//
//      sramReferenceDataVector.flatMap { vector =>
//        vector.find { data =>
//          data.capacityKb == (singleBufferLimitKb / bankCount) && data.bandwidthBits >= bandwidth
//        }
//      } match {
//        case Some(refData) => Some(DividedSramModel(bankCount, refData))
//        case None =>
//          throw SramBuildError(s"Cannot find the SRAM Size: ${(singleBufferLimitKb / bankCount)} KB and Bandwidth: ${bandwidth}")
//      }
//    }
//
//    val singleBufferLimitKb = dataType match {
//      case DataType.A => sramA.singleBufferLimitKb
//      case DataType.B => sramB.singleBufferLimitKb
//      case DataType.C => sramC.singleBufferLimitKb
//    }
//
//
//    sramReferenceDataVector.flatMap { vector =>
//      vector.find { data =>
//        data.capacityKb == singleBufferLimitKb
//      }
//    } match {
//      case Some(refData) => Some(DividedSramModel(1, refData))
//      case None =>
//        throw SramBuildError(s"Error in slice SRAM")
//    }
//
//  }

  private def calculateBankCount(arrayBandwidth: Int): Int = {
    val bandwidthRatio = offChipMemory.outputBandwidth.toDouble / arrayBandwidth.toDouble
    if (bandwidthRatio <= 1.0) return 1

    val powerOf2 = math.floor(math.log(bandwidthRatio) / math.log(2)).toInt
    math.pow(2, powerOf2).toInt
  }

  //Util functions
  private def areAllHardwareQueueEmpty: Boolean = {
    offChipMemory.isHardwareEmpty &&
      sramA.isHardwareEmpty &&
      sramB.isHardwareEmpty &&
      sramC.isHardwareEmpty &&
      array.isHardwareEmpty
  }

  private def printCompilationState(cycle: Long): Unit = {
    log("")
    log(s"[Current Cycle: $cycle]")
    offChipMemory.printTiles()
    sramA.printTiles()
    sramB.printTiles()
    array.printTiles()
    sramC.printTiles()
    array.printSchedule()
    log("")
  }
}




package simulation

import scala.util.control.Breaks._

class SystemSimulator(
  val dram: Dram,
  val sramA: DoubleBufferSram,
  val sramB: DoubleBufferSram,
  val sramC: OutputDoubleBufferSram,
  val interface: Interface,
  val layer: Layer,
  val array: Array,
  val dramReferenceData: Option[DramReferenceData] = None,
  val sramDataReferenceVector: Option[Vector[SramReferenceData]] = None,
  val arrayReferenceData: Option[ArrayReferenceData] = None,
  val debugStartCycle: Long = 0,
  val debugEndCycle: Long = 0,
  val debugMode: Boolean = false,
  val loggerOption: LoggerOption
) extends Logger {

  assert(areAllHardwareQueueEmpty, "[error] Hardware is not empty")
  setMode(loggerOption)

  private var cycle: Long = -1
  private val clockPeriod: Double = 2e-9

  def startSimulation(): Unit = {
    cycle = runSimulationLoop()
  }

  private def runSimulationLoop() : Long  = {

    var cycle: Long = 0

    require(layer.operationVector.nonEmpty, "Empty operation vector function is called in wrong place")
    dram.initDram(layer.operationVector, array.arrayConfig.dataflow)
    array.uploadOperationVector(layer.operationVector)
    sramA.initTileSchedule(layer.operationVector)
    sramB.initTileSchedule(layer.operationVector)

    breakable {
      while(!areAllHardwareQueueEmpty) {
//        println(s"Cycle: $cycle")
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

            println(s"Failed: ${e.getMessage}")
            printCompilationState(cycle)
            sys.exit(1)

        }

        if(array.capacityLeftTileA <0) {
          Console.err.println("Capacity is a negative value")
          printCompilationState(cycle)
          break()
        }

        if(array.capacityLeftTileB() < 0 ) {
          Console.err.println("Capacity is a negative value")
          printCompilationState(cycle)
          break()
        }

//        if(sramA.swapCount == 5){
//          Console.err.println("SEX")
//          log(s"Swap Count: ${sramA.swapCount}")
//          printCompilationState(cycle)
//          break()
//        }

        cycle += 1

      }
    }

//    println(s"Dim Size A: ${array.schedule.map(_.getTileA.dims.memorySize).sum}")
//    println(s"Dim Size B: ${array.schedule.map(_.getTileB.dims.memorySize).sum}")
//    println(s"Dim Size C: ${array.schedule.map(_.getTileC.dims.memorySize).sum}")
//    printCompilationState(cycle)
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

  //5. Energy Report
  // SRAM A
  def getSramReadEnergyA: Option[Double] =
    if (sramDataReferenceVector.isEmpty) None
    else Some(sramA.getReadAccessCount * findMatchingSramData(sramA, array.arrayConfig.bandwidthOfInputA).readEnergyPj)

  def getSramWriteEnergyA: Option[Double] =
    if (sramDataReferenceVector.isEmpty) None
    else Some(sramA.getWriteAccessCount * findMatchingSramData(sramA, array.arrayConfig.bandwidthOfInputA).writeEnergyPj)

  def getSramLeakageEnergyA: Option[Double] =
    if (sramDataReferenceVector.isEmpty) None
    else Some(findMatchingSramData(sramA, array.arrayConfig.bandwidthOfInputA).leakagePowerPw * cycle * clockPeriod)

  def getSramEnergyA: Option[Double] =
    if (sramDataReferenceVector.isEmpty) None
    else Some(getSramReadEnergyA.get + getSramWriteEnergyA.get + getSramLeakageEnergyA.get)

  // SRAM B
  def getSramReadEnergyB: Option[Double] =
    if (sramDataReferenceVector.isEmpty) None
    else Some(sramB.getReadAccessCount * findMatchingSramData(sramB, array.arrayConfig.bandwidthOfInputB).readEnergyPj)

  def getSramWriteEnergyB: Option[Double] =
    if (sramDataReferenceVector.isEmpty) None
    else Some(sramB.getWriteAccessCount * findMatchingSramData(sramB, array.arrayConfig.bandwidthOfInputB).writeEnergyPj)

  def getSramLeakageEnergyB: Option[Double] =
    if (sramDataReferenceVector.isEmpty) None
    else Some(findMatchingSramData(sramB, array.arrayConfig.bandwidthOfInputB).leakagePowerPw * cycle * clockPeriod)

  def getSramEnergyB: Option[Double] =
    if (sramDataReferenceVector.isEmpty) None
    else Some(getSramReadEnergyB.get + getSramWriteEnergyB.get + getSramLeakageEnergyB.get)

  // SRAM C
  def getSramReadEnergyC: Option[Double] =
    if (sramDataReferenceVector.isEmpty) None
    else Some(sramC.getReadAccessCount * findMatchingSramData(sramC, array.arrayConfig.outputBandwidth).readEnergyPj)

  def getSramWriteEnergyC: Option[Double] =
    if (sramDataReferenceVector.isEmpty) None
    else Some(sramC.getWriteAccessCount * findMatchingSramData(sramC, array.arrayConfig.outputBandwidth).writeEnergyPj)

  def getSramLeakageEnergyC: Option[Double] =
    if (sramDataReferenceVector.isEmpty) None
    else Some(findMatchingSramData(sramC, array.arrayConfig.outputBandwidth).leakagePowerPw * cycle * clockPeriod)

  def getSramEnergyC: Option[Double] =
    if (sramDataReferenceVector.isEmpty) None
    else Some(getSramReadEnergyC.get + getSramWriteEnergyC.get + getSramLeakageEnergyC.get)

  // DRAM
  def getDramReadEnergy: Option[Double] =
    if (dramReferenceData.isEmpty) None
    else Some(dram.getReadAccessCount * dramReferenceData.get.readEnergyPj)

  def getDramWriteEnergy: Option[Double] =
    if (dramReferenceData.isEmpty) None
    else Some(dram.getWriteAccessCount * dramReferenceData.get.writeEnergyPj)

  def getDramEnergy: Option[Double] =
    if (dramReferenceData.isEmpty) None
    else Some(getDramReadEnergy.get + getDramWriteEnergy.get)

  // Array
  def getArrayDynamicEnergy: Option[Double] =
    if (arrayReferenceData.isEmpty) None
    else Some(array.getArrayActiveCount.toDouble * computeArrayDynamicPower(arrayReferenceData.get))

  def getArrayLeakageEnergy: Option[Double] =
    if (arrayReferenceData.isEmpty) None
    else Some(computeArrayLeakagePower(arrayReferenceData.get) * cycle * clockPeriod)

  def getArrayEnergy: Option[Double] =
    if (arrayReferenceData.isEmpty) None
    else Some(getArrayDynamicEnergy.get + getArrayLeakageEnergy.get)

  // Total Energy
  def getTotalEnergy: Option[Double] =
    if (sramDataReferenceVector.isEmpty || dramReferenceData.isEmpty || arrayReferenceData.isEmpty) None
    else Some(
      getSramEnergyA.get + getSramEnergyB.get + getSramEnergyC.get +
        getDramEnergy.get + getArrayEnergy.get
    )

  //6. Area Report
  def getSramAreaA: Option[Double] =
    if(sramDataReferenceVector.isEmpty) None
    else Some(findMatchingSramData(sramA, array.arrayConfig.bandwidthOfInputA).areaUm2)

  def getSramAreaB: Option[Double] =
    if(sramDataReferenceVector.isEmpty) None
    else Some(findMatchingSramData(sramB, array.arrayConfig.bandwidthOfInputB).areaUm2)

  def getSramAreaC: Option[Double] =
    if(sramDataReferenceVector.isEmpty) None
    else Some(findMatchingSramData(sramC, array.arrayConfig.outputBandwidth).areaUm2)

  def getArrayArea: Option[Double] =
    if(sramDataReferenceVector.isEmpty) None
    else Some(findMatchingSramData(sramC, array.arrayConfig.outputBandwidth).areaUm2)

  def getArea: Option[Double] =
    if(arrayReferenceData.isEmpty) None
    else Some(computeArrayArea(arrayReferenceData.get))

  // Helper Function
  private def findMatchingSramData(sram: Sram, bandwidth: Int): SramReferenceData = {
    sramDataReferenceVector.get.find { sramData =>
      sramData.capacityKb == sram.singleBufferLimitKb &&
        bandwidth < sramData.bandwidthBits
    }.getOrElse(
      throw RunTimeError("No matching SRAM data found")
    )
  }

  private def computeArrayDynamicPower(data: ArrayReferenceData): Double = {
    array.arrayConfig.groupPeRow * data.dynamicPowerGroupPeRowPj +
      array.arrayConfig.groupPeCol * data.dynamicPowerGroupPeColPj +
      array.arrayConfig.vectorPeRow * data.dynamicPowerVectorPeRowPj +
      array.arrayConfig.vectorPeCol * data.dynamicPowerVectorPeColPj +
      array.arrayConfig.numMultiplier * data.dynamicPowerNumMultiplierPj
  }

  private def computeArrayLeakagePower(data: ArrayReferenceData): Double = {
    array.arrayConfig.groupPeRow * data.leakagePowerGroupPeRowPw +
      array.arrayConfig.groupPeCol * data.leakagePowerGroupPeColPw +
      array.arrayConfig.vectorPeRow * data.leakagePowerVectorPeRowPw +
      array.arrayConfig.vectorPeCol * data.leakagePowerVectorPeColPw +
      array.arrayConfig.numMultiplier * data.leakagePowerNumMultiplierPw
  }

  private def computeArrayArea(data:ArrayReferenceData): Double = {
    array.arrayConfig.groupPeRow * data.areaPowerGroupPeRowUm2 +
      array.arrayConfig.groupPeCol * data.areaPowerGroupPeColUm2 +
      array.arrayConfig.vectorPeRow * data.areaPowerVectorPeRowUm2 +
      array.arrayConfig.vectorPeCol * data.areaPowerVectorPeColUm2 +
      array.arrayConfig.numMultiplier * data.areaPowerNumMultiplierUm2
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
    sramA.printSchedule()
    sramB.printSchedule()

    log("")
  }
}



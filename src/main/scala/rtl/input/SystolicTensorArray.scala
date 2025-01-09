package rtl.input

import chisel3._
import common.ArrayDimension
import rtl.commonRtl.{Arithmetic, PortConfig, VerilogNaming}

class SystolicTensorArray[T <: Data](
  groupPeRow: Int,
  groupPeCol : Int,
  vectorPeRow : Int,
  vectorPeCol : Int,
  numMultiplier : Int,
  portConfig: PortConfig[T],
//  generateRtl: Boolean
)( implicit ev: Arithmetic[T] ) extends Module with VerilogNaming{

  override val desiredName:String = camelToSnake(this.getClass.getSimpleName)

  def this(arrayDimension: ArrayDimension, portConfig: PortConfig[T])(implicit ev: Arithmetic[T]) =
    this(
      arrayDimension.groupPeRow,
      arrayDimension.groupPeCol,
      arrayDimension.vectorPeRow,
      arrayDimension.vectorPeCol,
      arrayDimension.numMultiplier,
      portConfig,
    )

  val numInputA: Int = groupPeRow * vectorPeRow * numMultiplier
  val numInputB: Int = groupPeCol * vectorPeCol * numMultiplier
  val numPropagateA: Int = groupPeCol * vectorPeCol
  val numOutput : Int = groupPeRow * vectorPeRow
  val outputTypeC: T = portConfig.getStaOutputTypeC

  val processingElementVector = Vector.tabulate(groupPeRow, groupPeCol){ (rowIndex, colIndex) =>

    val isLastRow = rowIndex == groupPeRow - 1
    val isFirstCol = colIndex == 0
    val isLastCol = colIndex == groupPeCol - 1

    val withOutputA = !isLastCol
    val withOutputB = !isLastRow
    val withInputC = !isFirstCol


    if(vectorPeRow == 1 && vectorPeCol == 1){

      Module(new VectorProcessingElement(
        groupPeColIndex = 0,
        vectorPeColIndex = colIndex,
        vectorPeCol = 0,
        numMultiplier = numMultiplier,
        withOutputA = withOutputA,
        withOutputB = withOutputB,
        withInputC = withInputC,
        portConfig = portConfig,
      ))

    } else {

      Module(new GroupProcessingElement(
        groupPeColIndex = colIndex,
        vectorPeRow = vectorPeRow,
        vectorPeCol = vectorPeCol,
        numMultiplier = numMultiplier,
        withOutputA = withOutputA,
        withOutputB = withOutputB,
        withInputC = withInputC,
        portConfig = portConfig
      ))

    } : ProcessingElementIo[T]




  }

  val io = IO(new Bundle {
    val inputA = Input(Vec(numInputA, portConfig.inputTypeA))
    val inputB = Input(Vec(numInputB, portConfig.inputTypeB))
    val propagateA  = Input(Vec(numPropagateA, Bool()))
    val outputC = Output(Vec(numOutput, outputTypeC))
  })

  //Wiring Input A
  for( r <- 0 until groupPeRow)
    for( a <- 0 until vectorPeRow)
      for( p <- 0 until numMultiplier) {
        val multiplierIndex = a * numMultiplier + p
        processingElementVector(r)(0).io.inputA(multiplierIndex) := io.inputA(multiplierIndex + (r * vectorPeRow * numMultiplier))
      }

  //Wiring Input B
  for( c <- 0 until groupPeCol)
    for( b <- 0 until vectorPeCol)
      for( p <- 0 until numMultiplier) {
        val multiplierIndex = b * numMultiplier + p
        processingElementVector(0)(c).io.inputB(multiplierIndex) := io.inputB( multiplierIndex + (c * vectorPeCol * numMultiplier ))
      }

  for( r <- 0 until groupPeRow )
    for( c <- 0 until groupPeCol ) {
      val pe = processingElementVector(r)(c)
      pe.io.propagateA match{
        case vec: Vec[_] =>
          for ( b <- 0 until vectorPeCol)
            vec(b) := io.propagateA(b + c * vectorPeCol)

        case bool: Bool=>
          bool := io.propagateA(c)

        case _ =>
          throw new Exception("Wrong Type")
      }
    }

  //Wiring Output A
  for( r <- 0 until groupPeRow)
    for( c <- 1 until groupPeCol)
      for( a <- 0 until vectorPeRow)
        for( p <- 0 until numMultiplier) {
          val multiplierIndex = a * numMultiplier + p
          processingElementVector(r)(c).io.inputA(multiplierIndex) := processingElementVector(r)(c - 1).io.outputA.get(multiplierIndex)
        }

  //Wiring Output B
  for( r <- 1 until groupPeRow )
    for( c <- 0 until groupPeCol )
      for( b <- 0 until vectorPeCol )
        for( p <- 0 until numMultiplier ) {
          val multiplierIndex = b * numMultiplier + p
          processingElementVector(r)(c).io.inputB(multiplierIndex) := processingElementVector(r-1)(c).io.outputB.get(multiplierIndex)
        }

  //Wiring Output
//  for( r <- 0 until groupPeRow )
//    for( c <- 1 until groupPeCol )
//      for( a <- 0 until vectorPeRow )
//        groupProcessingElementVector(r)(c).io.inputC.get(a) := groupProcessingElementVector(r)(c - 1).io.outputC(a)

  for( r <- 0 until groupPeRow)
    for( c <- 1 until groupPeCol)
      (processingElementVector(r)(c).io.inputC, processingElementVector(r)(c-1).io.outputC) match {

        case( Some(inputC: Vec[_]), outputC: Vec[_] ) =>
          for( a <- 0 until vectorPeRow)
            inputC(a) := outputC(a)

        case( Some(inputC: Data), outputC: Data ) =>
          inputC := outputC

        case _ =>
          throw new Exception("wrong type")

      }

  for( r <- 0 until groupPeRow){
    val lastPe = processingElementVector(r)(groupPeCol - 1)
    lastPe.io.outputC match {
      case vec: Vec[_] =>
        for ( a<- 0 until vectorPeRow)
          io.outputC(a + (r * vectorPeRow)) := vec(a)

      case data: Data =>
        io.outputC(r) := data

      case _ =>
        throw new Exception("wrong type")
    }
  }

}

//package simulation
//
//object TileSizeRegistry {
//
//  private var tileSizeA: Option[Int] = None
//  private var tileSizeB: Option[Int] = None
//  private var tileSizeC: Option[Int] = None
//
//  def setTileSize(dataType: DataType.Value, size: Int): Unit = {
//    require(size > 0, "[error] Tile size must not be a negative integer")
//    dataType match {
//      case DataType.A => tileSizeA = Some(size)
//      case DataType.B => tileSizeB = Some(size)
//      case DataType.C => tileSizeC = Some(size)
//    }
//  }
//
//  def getTileSize(dataType: DataType.Value): Option[Int] = {
//    dataType match {
//      case DataType.A => tileSizeA
//      case DataType.B => tileSizeB
//      case DataType.C => tileSizeC
//    }
//  }
//
//  def isInitialized: Boolean = tileSizeA.isDefined && tileSizeB.isDefined && tileSizeC.isDefined
//
//}

package simulation

import common.{Dataflow, IntegerType, VerilogGenerationConfig, ArrayDimension}

object large extends App with RtlGenerationManager {

  private val is16x16x2x2x4VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Is,
    arrayDimension = new ArrayDimension(16, 16, 2, 2, 4),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Is,
      groupPeRow = 16,
      groupPeCol = 16,
      vectorPeRow = 2,
      vectorPeCol = 2,
      numMultiplier = 4,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val is8x32x2x4x2VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Is,
    arrayDimension = new ArrayDimension(8, 32, 2, 4, 2),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Is,
      groupPeRow = 8,
      groupPeCol = 32,
      vectorPeRow = 2,
      vectorPeCol = 4,
      numMultiplier = 2,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val is32x8x2x2x2VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Is,
    arrayDimension = new ArrayDimension(32, 8, 2, 2, 2),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Is,
      groupPeRow = 32,
      groupPeCol = 8,
      vectorPeRow = 2,
      vectorPeCol = 2,
      numMultiplier = 2,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val is16x8x4x2x4VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Is,
    arrayDimension = new ArrayDimension(16, 8, 4, 2, 4),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Is,
      groupPeRow = 16,
      groupPeCol = 8,
      vectorPeRow = 4,
      vectorPeCol = 2,
      numMultiplier = 4,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val is8x8x2x2x16VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Is,
    arrayDimension = new ArrayDimension(8, 8, 2, 2, 16),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Is,
      groupPeRow = 8,
      groupPeCol = 8,
      vectorPeRow = 2,
      vectorPeCol = 2,
      numMultiplier = 16,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val os8x64x1x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(8, 64, 1, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 8,
      groupPeCol = 64,
      vectorPeRow = 1,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 256
    ),
  )

  private val os64x8x1x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(64, 8, 1, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 64,
      groupPeCol = 8,
      vectorPeRow = 1,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 512
    ),
  )

  private val os16x16x2x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(16, 16, 2, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 16,
      groupPeCol = 16,
      vectorPeRow = 2,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1024
    ),
  )

  private val os8x16x4x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(8, 16, 4, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 8,
      groupPeCol = 16,
      vectorPeRow = 4,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 256
    ),
  )

  private val os8x8x8x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(8, 8, 8, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 8,
      groupPeCol = 8,
      vectorPeRow = 8,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 128
    ),
  )

  private val os16x8x2x8x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(16, 8, 2, 8, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 16,
      groupPeCol = 8,
      vectorPeRow = 2,
      vectorPeCol = 8,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 256
    ),
  )

  private val ws16x16x1x8x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Ws,
    arrayDimension = new ArrayDimension(16, 16, 1, 8, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Ws,
      groupPeRow = 16,
      groupPeCol = 16,
      vectorPeRow = 1,
      vectorPeCol = 8,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val ws8x16x4x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Ws,
    arrayDimension = new ArrayDimension(8, 16, 4, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Ws,
      groupPeRow = 8,
      groupPeCol = 16,
      vectorPeRow = 4,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val ws8x8x8x2x8VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Ws,
    arrayDimension = new ArrayDimension(8, 8, 8, 2, 8),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Ws,
      groupPeRow = 8,
      groupPeCol = 8,
      vectorPeRow = 8,
      vectorPeCol = 2,
      numMultiplier = 8,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val ws32x8x1x8x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Ws,
    arrayDimension = new ArrayDimension(32, 8, 1, 8, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Ws,
      groupPeRow = 32,
      groupPeCol = 8,
      vectorPeRow = 1,
      vectorPeCol = 8,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  // 4096 Multipliers 구성

  private val is16x16x4x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Is,
    arrayDimension = new ArrayDimension(16, 16, 4, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Is,
      groupPeRow = 16,
      groupPeCol = 16,
      vectorPeRow = 4,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val is8x8x8x8x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Is,
    arrayDimension = new ArrayDimension(8, 8, 8, 8, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Is,
      groupPeRow = 8,
      groupPeCol = 8,
      vectorPeRow = 8,
      vectorPeCol = 8,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val is64x16x1x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Is,
    arrayDimension = new ArrayDimension(64, 16, 1, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Is,
      groupPeRow = 64,
      groupPeCol = 16,
      vectorPeRow = 1,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val is32x16x2x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Is,
    arrayDimension = new ArrayDimension(32, 16, 2, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Is,
      groupPeRow = 32,
      groupPeCol = 16,
      vectorPeRow = 2,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val is8x8x4x2x16VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Is,
    arrayDimension = new ArrayDimension(8, 8, 4, 2, 16),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Is,
      groupPeRow = 8,
      groupPeCol = 8,
      vectorPeRow = 4,
      vectorPeCol = 2,
      numMultiplier = 16,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val is8x32x2x8x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Is,
    arrayDimension = new ArrayDimension(8, 32, 2, 8, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Is,
      groupPeRow = 8,
      groupPeCol = 32,
      vectorPeRow = 2,
      vectorPeCol = 8,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val os16x32x2x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(16, 32, 2, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 16,
      groupPeCol = 32,
      vectorPeRow = 2,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 64
    ),
  )

  private val os32x16x2x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(32, 16, 2, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 32,
      groupPeCol = 16,
      vectorPeRow = 2,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 512
    ),
  )

  private val os8x8x16x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(8, 8, 16, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 8,
      groupPeCol = 8,
      vectorPeRow = 16,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 128
    ),
  )

  private val os8x128x1x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(8, 128, 1, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 8,
      groupPeCol = 128,
      vectorPeRow = 1,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1024
    ),
  )

  private val os128x8x1x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(128, 8, 1, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 128,
      groupPeCol = 8,
      vectorPeRow = 1,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 128
    ),
  )

  private val os16x8x4x8x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(16, 8, 4, 8, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 16,
      groupPeCol = 8,
      vectorPeRow = 4,
      vectorPeCol = 8,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 256
    ),
  )

  private val os8x16x4x8x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(8, 16, 4, 8, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 8,
      groupPeCol = 16,
      vectorPeRow = 4,
      vectorPeCol = 8,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 2048
    ),
  )

  private val ws16x32x2x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Ws,
    arrayDimension = new ArrayDimension(16, 32, 2, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Ws,
      groupPeRow = 16,
      groupPeCol = 32,
      vectorPeRow = 2,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val ws32x16x2x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Ws,
    arrayDimension = new ArrayDimension(32, 16, 2, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Ws,
      groupPeRow = 32,
      groupPeCol = 16,
      vectorPeRow = 2,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val ws8x8x8x8x4VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Ws,
    arrayDimension = new ArrayDimension(8, 8, 8, 8, 4),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Ws,
      groupPeRow = 8,
      groupPeCol = 8,
      vectorPeRow = 8,
      vectorPeCol = 8,
      numMultiplier = 4,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val ws64x8x2x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Ws,
    arrayDimension = new ArrayDimension(64, 8, 2, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Ws,
      groupPeRow = 64,
      groupPeCol = 8,
      vectorPeRow = 2,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val ws8x64x1x8x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Ws,
    arrayDimension = new ArrayDimension(8, 64, 1, 8, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Ws,
      groupPeRow = 8,
      groupPeCol = 64,
      vectorPeRow = 1,
      vectorPeCol = 8,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  // 16384 Multipliers 구성
  private val is32x32x4x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Is,
    arrayDimension = new ArrayDimension(32, 32, 4, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Is,
      groupPeRow = 32,
      groupPeCol = 32,
      vectorPeRow = 4,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val is16x32x8x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Is,
    arrayDimension = new ArrayDimension(16, 32, 8, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Is,
      groupPeRow = 16,
      groupPeCol = 32,
      vectorPeRow = 8,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val is64x32x2x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Is,
    arrayDimension = new ArrayDimension(64, 32, 2, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Is,
      groupPeRow = 64,
      groupPeCol = 32,
      vectorPeRow = 2,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val is128x16x2x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Is,
    arrayDimension = new ArrayDimension(128, 16, 2, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Is,
      groupPeRow = 128,
      groupPeCol = 16,
      vectorPeRow = 2,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val is8x16x8x8x16VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Is,
    arrayDimension = new ArrayDimension(8, 16, 8, 8, 16),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Is,
      groupPeRow = 8,
      groupPeCol = 16,
      vectorPeRow = 8,
      vectorPeCol = 8,
      numMultiplier = 16,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val os32x32x4x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(32, 32, 4, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 32,
      groupPeCol = 32,
      vectorPeRow = 4,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 128
    ),
  )

  private val os16x64x4x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(16, 64, 4, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 16,
      groupPeCol = 64,
      vectorPeRow = 4,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 4096
    ),
  )

  private val os64x16x4x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(64, 16, 4, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 64,
      groupPeCol = 16,
      vectorPeRow = 4,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 256
    ),
  )

  private val os32x8x16x8x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(32, 8, 16, 8, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 32,
      groupPeCol = 8,
      vectorPeRow = 16,
      vectorPeCol = 8,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 128
    ),
  )

  private val os8x32x16x8x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(8, 32, 16, 8, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 8,
      groupPeCol = 32,
      vectorPeRow = 16,
      vectorPeCol = 8,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1024
    ),
  )

  private val os128x8x4x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(128, 8, 4, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 128,
      groupPeCol = 8,
      vectorPeRow = 4,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 512
    ),
  )

  private val os8x128x4x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Os,
    arrayDimension = new ArrayDimension(8, 128, 4, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Os,
      groupPeRow = 8,
      groupPeCol = 128,
      vectorPeRow = 4,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1024
    ),
  )

  private val ws32x64x2x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Ws,
    arrayDimension = new ArrayDimension(32, 64, 2, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Ws,
      groupPeRow = 32,
      groupPeCol = 64,
      vectorPeRow = 2,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val ws16x8x16x8x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Ws,
    arrayDimension = new ArrayDimension(16, 8, 16, 8, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Ws,
      groupPeRow = 16,
      groupPeCol = 8,
      vectorPeRow = 16,
      vectorPeCol = 8,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val ws128x8x4x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Ws,
    arrayDimension = new ArrayDimension(128, 8, 4, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Ws,
      groupPeRow = 128,
      groupPeCol = 8,
      vectorPeRow = 4,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val ws8x16x16x8x2VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Ws,
    arrayDimension = new ArrayDimension(8, 16, 16, 8, 2),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Ws,
      groupPeRow = 8,
      groupPeCol = 16,
      vectorPeRow = 16,
      vectorPeCol = 8,
      numMultiplier = 2,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val ws8x128x4x4x1VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Ws,
    arrayDimension = new ArrayDimension(8, 128, 4, 4, 1),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Ws,
      groupPeRow = 8,
      groupPeCol = 128,
      vectorPeRow = 4,
      vectorPeCol = 4,
      numMultiplier = 1,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val ws8x16x8x4x8VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Ws,
    arrayDimension = new ArrayDimension(8, 16, 8, 4, 8),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Ws,
      groupPeRow = 8,
      groupPeCol = 16,
      vectorPeRow = 8,
      vectorPeCol = 4,
      numMultiplier = 8,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  private val ws16x16x8x4x4VerilogConfig = VerilogGenerationConfig(
    splitVerilogOutput = false,
    dataflow = Dataflow.Ws,
    arrayDimension = new ArrayDimension(16, 16, 8, 4, 4),
    integerType = IntegerType.Signed,
    portBitWidthInfo = calculatePortBitWidthInfo(
      dataflow = Dataflow.Ws,
      groupPeRow = 16,
      groupPeCol = 16,
      vectorPeRow = 8,
      vectorPeCol = 4,
      numMultiplier = 4,
      bitWidthPortA = 8,
      bitWidthPortB = 8,
      configPortC = None,
      streamingDimensionSize = 1
    ),
  )

  // 모든 구성에 대해 generateRtl 호출
//  generateRtl(is16x16x2x2x4VerilogConfig)
//  generateRtl(is8x32x2x4x2VerilogConfig)
//  generateRtl(is32x8x2x2x2VerilogConfig)
//  generateRtl(is16x8x4x2x4VerilogConfig)
//  generateRtl(is8x8x2x2x16VerilogConfig)
//  generateRtl(os8x64x1x4x1VerilogConfig)
//  generateRtl(os64x8x1x4x1VerilogConfig)
//  generateRtl(os16x16x2x4x1VerilogConfig)
//  generateRtl(os8x16x4x4x1VerilogConfig)
//  generateRtl(os8x8x8x4x1VerilogConfig)
//  generateRtl(os16x8x2x8x1VerilogConfig)
//  generateRtl(ws16x16x1x8x1VerilogConfig)
//  generateRtl(ws8x16x4x4x1VerilogConfig)
//  generateRtl(ws8x8x8x2x8VerilogConfig)
//  generateRtl(ws32x8x1x8x1VerilogConfig)
//
//  // 4096 Multipliers 구성
//  generateRtl(is16x16x4x4x1VerilogConfig)
//  generateRtl(is8x8x8x8x1VerilogConfig)
//  generateRtl(is64x16x1x4x1VerilogConfig)
//  generateRtl(is32x16x2x4x1VerilogConfig)
//  generateRtl(is8x8x4x2x16VerilogConfig)
//  generateRtl(is8x32x2x8x1VerilogConfig)
//  generateRtl(os16x32x2x4x1VerilogConfig)
//  generateRtl(os32x16x2x4x1VerilogConfig)
//  generateRtl(os8x8x16x4x1VerilogConfig)
//  generateRtl(os8x128x1x4x1VerilogConfig)
//  generateRtl(os128x8x1x4x1VerilogConfig)
//  generateRtl(os16x8x4x8x1VerilogConfig)
//  generateRtl(os8x16x4x8x1VerilogConfig)
//  generateRtl(ws16x32x2x4x1VerilogConfig)
//  generateRtl(ws32x16x2x4x1VerilogConfig)
//  generateRtl(ws8x8x8x8x4VerilogConfig)
//  generateRtl(ws64x8x2x4x1VerilogConfig)
//  generateRtl(ws8x64x1x8x1VerilogConfig)

  // 16384 Multipliers 구성
  generateRtl(is32x32x4x4x1VerilogConfig)
  generateRtl(is16x32x8x4x1VerilogConfig)
  generateRtl(is64x32x2x4x1VerilogConfig)
  generateRtl(is128x16x2x4x1VerilogConfig)
  generateRtl(is8x16x8x8x16VerilogConfig)
  generateRtl(os32x32x4x4x1VerilogConfig)
  generateRtl(os16x64x4x4x1VerilogConfig)
  generateRtl(os64x16x4x4x1VerilogConfig)
  generateRtl(os32x8x16x8x1VerilogConfig)
  generateRtl(os8x32x16x8x1VerilogConfig)
  generateRtl(os128x8x4x4x1VerilogConfig)
  generateRtl(os8x128x4x4x1VerilogConfig)
  generateRtl(ws32x64x2x4x1VerilogConfig)
  generateRtl(ws16x8x16x8x1VerilogConfig)
  generateRtl(ws128x8x4x4x1VerilogConfig)
  generateRtl(ws8x16x16x8x2VerilogConfig)
  generateRtl(ws8x128x4x4x1VerilogConfig)
  generateRtl(ws8x16x8x4x8VerilogConfig)
  generateRtl(ws16x16x8x4x4VerilogConfig)
}

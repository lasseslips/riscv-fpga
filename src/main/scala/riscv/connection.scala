package riscv
import chisel3._

class FeDec extends Bundle {
  val pc = UInt(32.W)
  val instruction = UInt(32.W)
}

class DecEx extends Bundle {
  val imm = UInt(32.W)
  val regData1 = UInt(32.W)
  val regData2 = UInt(32.W)
  val pc = UInt(32.W)

  val regWrite = Bool()
  val memWrite = Bool()
  val memIns = UInt(3.W)
  val regWriteSrc = UInt(2.W)
  val aluSrc = UInt(2.W)
  val aluOpcode = UInt(5.W)
  val branchEnable = Bool()
  val branchType = UInt(3.W)
  val jumpEnable = Bool()
}

class ExMem extends Bundle {
  val data = UInt(32.W)
  val addr = UInt(32.W)
  val wrType = UInt(3.W)
  val pc = UInt(32.W)
}

class MemWb extends Bundle {
  val alu = UInt(32.W)
  val mem = UInt(32.W)
  val pc = UInt(32.W)
}

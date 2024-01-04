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
  val aluSrc = UInt(2.W) //
  val aluOpcode = UInt(5.W) //
  val branchEnable = Bool() //
  val branchType = UInt(3.W) //
  val jumpEnable = Bool() //

  val regWrIdx = UInt(5.W)

}

class ExMem extends Bundle {
  val data = UInt(32.W)
  val addr = UInt(32.W)
  val pc = UInt(32.W)

  val regWrite = Bool()
  val memWrite = Bool()
  val memIns = UInt(3.W)

  val regWrIdx = UInt(5.W)
  val regWriteSrc = UInt(2.W)
}

class MemWb extends Bundle {
  val alu = UInt(32.W)
  val mem = UInt(32.W)
  val pc = UInt(32.W)
  
  val regWrite = Bool()
  val regWrIdx = UInt(5.W)
  val regWriteSrc = UInt(2.W)
}

class WbDec extends Bundle {
  val wrData = UInt(32.W)
  val regWrIdx = UInt(5.W)
  val regWrite = Bool()
}

class ExFe extends Bundle {
  val pc = UInt(32.W)
  val jump = Bool()
}

object Zeroed {
  def FeDec() = {
    val bundle = Wire(new FeDec)
    bundle.pc := 0.U
    bundle.instruction := 0.U
    bundle
  }

  def DecEx() = {
    val bundle = Wire(new DecEx)
    bundle.imm := 0.U
    bundle.regData1 := 0.U
    bundle.regData2 := 0.U
    bundle.pc := 0.U

    bundle.regWrite := false.B
    bundle.memWrite := false.B
    bundle.memIns := 0.U
    bundle.regWriteSrc := 0.U
    bundle.aluSrc := 0.U
    bundle.aluOpcode := 0.U
    bundle.branchEnable := false.B
    bundle.branchType := 0.U
    bundle.jumpEnable := false.B

    bundle.regWrIdx := 0.U
    bundle
  }

  def ExMem() = {
    val bundle = Wire(new ExMem)
    bundle.data := 0.U
    bundle.addr := 0.U
    bundle.pc := 0.U

    bundle.regWrite := false.B
    bundle.memWrite := false.B
    bundle.memIns := 0.U

    bundle.regWrIdx := 0.U
    bundle.regWriteSrc := 0.U
    bundle
  }

  def MemWb() = {
    val bundle = Wire(new MemWb)
    bundle.alu := 0.U
    bundle.mem := 0.U
    bundle.pc := 0.U

    bundle.regWrite := false.B
    bundle.regWrIdx := 0.U
    bundle.regWriteSrc := 0.U
    bundle
  }

  def WbDec() = {
    val bundle = Wire(new WbDec)
    bundle.wrData := 0.U
    bundle.regWrIdx := 0.U
    bundle.regWrite := false.B
    bundle
  }

  def ExFe() = {
    val bundle = Wire(new ExFe)
    bundle.pc := 0.U
    bundle.jump := false.B
    bundle
  }
}

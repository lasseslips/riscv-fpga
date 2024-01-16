package riscv.stages

import chisel3._
import chisel3.util._
import riscv.lib.{ExMem, LoadStoreFunct, MemWb}
class DataMemory extends Module {
  val io = IO(new Bundle() {
    val ExMem = Input(new ExMem())
    val MemWb = Output(new MemWb())
    val MemAddr = Output(UInt(32.W))
  })

  val dataOut = Wire(UInt(32.W))
  val dataIn = Wire(UInt(32.W))
  val memIns = Wire(UInt(3.W))
  val addr = Wire(UInt(32.W))
  val memWrite = Wire(Bool())
  val data = Wire(Vec(4, UInt(8.W)))
  val memDataReg = RegNext(io.ExMem.addr)
  io.MemAddr := memDataReg

  class ControlReg extends Bundle {
    val regWrite = Bool()
    val regWrIdx = UInt(5.W)
    val regWriteSrc = UInt(2.W)
    val pc = UInt(32.W)
  }
  val controlRegValues = Wire(new ControlReg)
  controlRegValues.regWrite := io.ExMem.regWrite
  controlRegValues.regWrIdx := io.ExMem.regWrIdx
  controlRegValues.regWriteSrc := io.ExMem.regWriteSrc
  controlRegValues.pc := io.ExMem.pc
  val controlReg = RegNext(controlRegValues)


  dataOut := DontCare

  memIns := io.ExMem.memIns
  dataIn := io.ExMem.data
  addr := io.ExMem.addr
  memWrite := io.ExMem.memWrite
  val memInsReg = RegNext(memIns)

  val dataInVec = Wire(Vec(4, UInt(8.W)))
  dataInVec(0) := dataIn(7,0)
  dataInVec(1) := dataIn(15,8)
  dataInVec(2) := dataIn(23,16)
  dataInVec(3) := dataIn(31,24)

  val mask = Wire(Vec(4, Bool()))
  mask := DontCare
  val signBit = Wire(Bool())
  signBit := DontCare


  switch(memIns) {
    is(LoadStoreFunct.LB_SB.U) {
      mask(0) := true.B
      mask(1) := false.B
      mask(2) := false.B
      mask(3) := false.B
    }
    is(LoadStoreFunct.LH_SH.U) {
      mask(0) := true.B
      mask(1) := true.B
      mask(2) := false.B
      mask(3) := false.B
    }
    is(LoadStoreFunct.LW_SW.U) {
      mask(0) := true.B
      mask(1) := true.B
      mask(2) := true.B
      mask(3) := true.B
    }
    is(LoadStoreFunct.LBU.U) {
      mask(0) := true.B
      mask(1) := false.B
      mask(2) := false.B
      mask(3) := false.B
    }
    is(LoadStoreFunct.LHU.U) {
      mask(0) := true.B
      mask(1) := true.B
      mask(2) := false.B
      mask(3) := false.B
    }
  }
  // 65536 x 32bit = 2,097,152 bit
  val mem = SyncReadMem(Math.pow(2, 16).toInt, Vec(4, UInt(8.W)), SyncReadMem.WriteFirst)

  when(memWrite) {
    mem.write(addr,dataInVec,mask)
  }
  data := mem.read(addr)


  //sw 00 00 00 a5 is stored as a negative number.

  switch(memInsReg) {
    is(LoadStoreFunct.LB_SB.U) {
      signBit := data(0)(7)
      dataOut := Cat(Fill(24,signBit), data(0))
    }
    is(LoadStoreFunct.LH_SH.U) {
      signBit := data(1)(7)
      dataOut := Cat(Fill(16,signBit), data(1),data(0))
    }
    is(LoadStoreFunct.LW_SW.U) {
      dataOut := Cat(data(3),data(2),data(1),data(0))
    }
    is(LoadStoreFunct.LBU.U) {
      signBit := false.B
      dataOut := Cat(Fill(24,signBit),data(0))
    }
    is(LoadStoreFunct.LHU.U) {
      signBit := false.B
      dataOut := Cat(Fill(16,signBit), data(1),data(0))
    }
  }

  val aluReg = RegNext(addr)
  io.MemWb.alu := aluReg
  io.MemWb.mem := dataOut
  io.MemWb.pc := controlReg.pc
  io.MemWb.regWrIdx := controlReg.regWrIdx
  io.MemWb.regWrite := controlReg.regWrite
  io.MemWb.regWriteSrc := controlReg.regWriteSrc



}

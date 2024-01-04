package riscv

import chisel3._
import chisel3.util._
class DataMemory extends Module {
  val io = IO(new Bundle() {
    val ExMem = Input(new ExMem())
    val MemWb = Output(new MemWb())
  })

  val dataOut = Wire(UInt(32.W))
  val dataIn = Wire(UInt(32.W))
  val memIns = Wire(UInt(3.W))
  val addr = Wire(UInt(32.W))
  val memWrite = Wire(Bool())

  class ControlReg extends Bundle {
    val regWrite = Bool()
    val regWrIdx = UInt(4.W)
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

  val dataInVec = Wire(Vec(4, UInt(8.W)))
  dataInVec(0) := dataIn(7,0)
  dataInVec(1) := dataIn(15,8)
  dataInVec(2) := dataIn(23,16)
  dataInVec(3) := dataIn(31,24)
  val dataOutVec = Wire(Vec(4,UInt(8.W)))

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
  // 1,048,576 x 32 bit = 32Mb
  val mem = SyncReadMem(Math.pow(2, 10).toInt, Vec(4, UInt(8.W)), SyncReadMem.WriteFirst)

  when(memWrite) {
    mem.write(addr,dataInVec,mask)
  }
  val data = mem.read(addr)

  for (i <- 0 until 4) {
    when(mask(i)) {
      dataOutVec(i) := data(i)
    } .otherwise {
      dataOutVec(i) := 0.U
    }
  }

  switch(memIns) {
    is(LoadStoreFunct.LB_SB.U) {
      signBit := dataOutVec(0)(7)
      dataOut := Cat(Fill(24,signBit), dataOutVec(0))
    }
    is(LoadStoreFunct.LH_SH.U) {
      signBit := dataOutVec(1)(7)
      dataOut := Cat(Fill(16,signBit), dataOutVec(1),dataOutVec(0))
    }
    is(LoadStoreFunct.LW_SW.U) {
      dataOut := Cat(dataOutVec(3),dataOutVec(2),dataOutVec(1),dataOutVec(0))
    }
    is(LoadStoreFunct.LBU.U) {
      signBit := false.B
      dataOut := Cat(Fill(24,signBit),dataOutVec(0))
    }
    is(LoadStoreFunct.LHU.U) {
      signBit := false.B
      dataOut := Cat(Fill(16,signBit), dataOutVec(1),dataOutVec(0))
    }
  }

  io.MemWb.alu := dataIn
  io.MemWb.mem := dataOut
  io.MemWb.pc := controlReg.pc
  io.MemWb.regWrIdx := controlReg.regWrIdx
  io.MemWb.regWrite := controlReg.regWrite
  io.MemWb.regWriteSrc := controlReg.regWriteSrc



}

package riscv

import chisel3._
import chisel3.util._
class DataMemory extends Module {
  val io = IO(new Bundle() {
    val ExMem = Input(new ExMem())
    val write = Input(Bool())
    val dataOut = Output(UInt(32.W))
    val MemWb = Output(new MemWb())
  })

  val dataOut = DontCare
  val dataIn = Wire(UInt(32.W))
  val wrType = Wire(UInt(3.W))
  val addr = Wire(UInt(32.W))

  wrType := io.ExMem.wrType
  dataIn := io.ExMem.data
  addr := io.ExMem.addr

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


  switch(wrType) {
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

  when(io.write) {
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

  switch(wrType) {
    is(LoadStoreFunct.LB_SB.U) {
      signBit := dataOutVec(0)(7)
      dataOut := Cat(Fill(24,signBit), dataOutVec(0))
    }
    is(LoadStoreFunct.LH_SH.U) {
      signBit := dataOutVec(1)(7)
      dataOut := Cat(Fill(16,signBit), dataOutVec(1),dataOutVec(0))
    }
    is(LoadStoreFunct.LW_SW.U) {
      dataOut := Cat(dataOutVec)
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
  io.MemWb.pc := io.ExMem.pc


}

package riscv

import chisel3._
import chisel3.util._
class DataMemory extends Module {
  val io = IO(new Bundle() {
    val addr = Input(UInt(20.W))
    val write = Input(Bool())
    val dataIn = Input(UInt(32.W))
    val dataOut = Output(UInt(32.W))
    val insType = Input(UInt(3.W))
  })

  io.dataOut := DontCare
  val dataInVec = Wire(Vec(4, UInt(8.W)))
  dataInVec(0) := io.dataIn(7,0)
  dataInVec(1) := io.dataIn(15,8)
  dataInVec(2) := io.dataIn(23,16)
  dataInVec(3) := io.dataIn(31,24)
  val dataOutVec = Wire(Vec(4,UInt(8.W)))

  val mask = Wire(Vec(4, Bool()))
  mask := DontCare
  val signBit = Wire(Bool())
  signBit := DontCare


  switch(io.insType) {
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

  io.dataOut := DontCare
  when(io.write) {
    mem.write(io.addr,dataInVec,mask)
  }
  val data = mem.read(io.addr)

  for (i <- 0 until 4) {
    when(mask(i)) {
      dataOutVec(i) := data(i)
    } .otherwise {
      dataOutVec(i) := 0.U
    }
  }

  switch(io.insType) {
    is(LoadStoreFunct.LB_SB.U) {
      signBit := dataOutVec(0)(7)
      io.dataOut := Cat(Fill(24,signBit), dataOutVec(0))
    }
    is(LoadStoreFunct.LH_SH.U) {
      signBit := dataOutVec(1)(7)
      io.dataOut := Cat(Fill(16,signBit), dataOutVec(1),dataOutVec(0))
    }
    is(LoadStoreFunct.LW_SW.U) {
      io.dataOut := Cat(dataOutVec)
    }
    is(LoadStoreFunct.LBU.U) {
      signBit := false.B
      io.dataOut := Cat(Fill(24,signBit),dataOutVec(0))
    }
    is(LoadStoreFunct.LHU.U) {
      signBit := false.B
      io.dataOut := Cat(Fill(16,signBit), dataOutVec(1),dataOutVec(0))
    }
  }

}

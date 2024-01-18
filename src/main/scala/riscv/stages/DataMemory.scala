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
  dataInVec := VecInit(Seq.fill(4)(0.U(8.W)))

  val mask = Wire(Vec(4, Bool()))
  mask := DontCare
  val signBit = Wire(Bool())
  signBit := DontCare

  val byteOffset = addr(1,0)

  when(memIns === LoadStoreFunct.LB_SB.U | memIns === LoadStoreFunct.LBU.U) {
    mask := VecInit(Seq.fill(4)(false.B))
    mask(byteOffset) := true.B
    dataInVec(byteOffset) := dataIn(7,0)
  } .elsewhen(memIns === LoadStoreFunct.LH_SH.U | memIns === LoadStoreFunct.LHU.U) {
    mask := VecInit(Seq.fill(4)(false.B))
    when(byteOffset === 3.U) {
      mask(byteOffset) := true.B
      dataInVec(byteOffset) := dataIn(7,0)
    } .otherwise {
      mask(byteOffset) := true.B
      mask(byteOffset + 1.U) := true.B
      dataInVec(byteOffset) := dataIn(7,0)
      dataInVec(byteOffset + 1.U) := dataIn(15,8)
    }
  } .elsewhen(memIns === LoadStoreFunct.LW_SW.U) {
    switch(byteOffset) {
      is(0.U) {
        mask(0) := true.B
        mask(1) := true.B
        mask(2) := true.B
        mask(3) := true.B
        dataInVec(0) := dataIn(7,0)
        dataInVec(1) := dataIn(15,8)
        dataInVec(2) := dataIn(23,16)
        dataInVec(3) := dataIn(31,24)
      }
      is(1.U) {
        mask(0) := false.B
        mask(1) := true.B
        mask(2) := true.B
        mask(3) := true.B
        dataInVec(1) := dataIn(7,0)
        dataInVec(2) := dataIn(15,8)
        dataInVec(3) := dataIn(23,16)
      }
      is(2.U) {
        mask(0) := false.B
        mask(1) := false.B
        mask(2) := true.B
        mask(3) := true.B
        dataInVec(2) := dataIn(7,0)
        dataInVec(3) := dataIn(15,8)
      }
      is(3.U) {
        mask(0) := false.B
        mask(1) := false.B
        mask(2) := false.B
        mask(3) := true.B
        dataInVec(3) := dataIn(7,0)
      }
    }
  }
  // 65536 x 32bit = 2,097,152 bit
  val mem = SyncReadMem(Math.pow(2, 16).toInt, Vec(4, UInt(8.W)), SyncReadMem.WriteFirst)

  when(memWrite) {
    mem.write((addr - byteOffset),dataInVec,mask)
  }
  data := mem.read(addr - byteOffset)



  val byteOffsetReg = RegNext(byteOffset)
  switch(memInsReg) {
    is(LoadStoreFunct.LB_SB.U) {
      signBit := data(byteOffsetReg)(7)
      dataOut := Cat(Fill(24,signBit), data(byteOffsetReg))
    }
    is(LoadStoreFunct.LH_SH.U) {
      signBit := data(byteOffsetReg + 1.U)(7)
      when(byteOffsetReg === 3.U) {
        dataOut := Cat(Fill(24,signBit), data(byteOffsetReg))
      } .otherwise {
        dataOut := Cat(Fill(16,signBit), data(byteOffsetReg+1.U),data(byteOffsetReg))
      }
    }
    is(LoadStoreFunct.LW_SW.U) {
      switch(byteOffsetReg) {
        is(0.U) {
          dataOut := Cat(data(3),data(2),data(1),data(0))
        }
        is(1.U) {
          dataOut := Cat(Fill(8,0.U),data(3),data(2),data(1))
        }
        is(2.U) {
          dataOut := Cat(Fill(16,0.U),data(3),data(2))
        }
        is(3.U) {
          dataOut := Cat(Fill(24,0.U),data(3))
        }
      }
    }
    is(LoadStoreFunct.LBU.U) {
      signBit := false.B
      dataOut := Cat(Fill(24,signBit),data(byteOffsetReg))
    }
    is(LoadStoreFunct.LHU.U) {
      signBit := false.B
      when(byteOffsetReg === 3.U) {
        dataOut := Cat(Fill(24,signBit), data(byteOffsetReg))
      } .otherwise {
        dataOut := Cat(Fill(16,signBit), data(byteOffsetReg+1.U),data(byteOffsetReg))
      }
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

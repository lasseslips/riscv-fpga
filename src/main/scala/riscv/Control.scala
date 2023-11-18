package riscv

import chisel3._
import chisel3.util._

class Control extends Module {
  val io = IO(new Bundle() {
    val insType = Input(UInt(6.W))
    val types = Input(UInt(5.W))
    val regWrite = Output(Bool())
    val memWrite = Output(Bool())
    val memIns = Output(UInt(3.W))
    val regWriteSrc = Output(UInt(2.W))
    val aluSrc = Output(UInt(2.W))
    val aluOpcode = Output(UInt(5.W))
    val branchEnable = Output(Bool())
    val branchType = Output(UInt(3.W))
    val jumpEnable = Output(Bool())
  })

  io.memWrite := false.B
  io.regWrite := false.B
  io.regWriteSrc := DontCare
  io.aluSrc := DontCare
  io.aluOpcode := DontCare
  io.jumpEnable := false.B
  io.branchEnable := false.B
  io.branchType := DontCare
  io.memIns := DontCare

  switch(io.types) {
    is(Types.R.id.U) {
      io.regWrite := true.B
      io.aluSrc := "b00".U
      io.regWriteSrc := RegWriteSrc.ALU.id.U
      io.aluOpcode := io.insType
    }
    is(Types.I.id.U) {
      io.regWrite := true.B
      io.aluSrc := "b01".U
      io.regWriteSrc := RegWriteSrc.ALU.id.U
      io.aluOpcode := io.insType
    }
    is(Types.B.id.U) {
      io.regWrite := false.B
      io.branchType := io.insType
      io.branchEnable := true.B
      io.aluSrc := "b11".U
      io.aluOpcode := insType.ADD.U
    }
    is(Types.LOAD.id.U) {
      io.regWrite := true.B
      io.aluSrc := "b01".U
      io.aluOpcode := insType.ADD.U
      io.memIns := io.insType(2,0)
      io.regWriteSrc := RegWriteSrc.MEMORY.id.U
    }
    is(Types.S.id.U) {
      io.memWrite := true.B
      io.regWrite := false.B
      io.aluSrc := "b01".U
      io.aluOpcode := insType.ADD.U
      io.regWriteSrc := RegWriteSrc.MEMORY.id.U
      io.memIns := io.insType(2,0)
    }
    is(Types.U.id.U) {
      io.regWrite := true.B
      io.aluOpcode := insType.ADD.U
      io.regWriteSrc := RegWriteSrc.ALU.id.U
      when(io.insType === insType.LUI.U) {
        io.aluSrc := "b01".U
      } .otherwise {
        io.aluSrc := "b11".U
      }

    }
    is(Types.J.id.U) {
      when(io.insType === insType.JAL.U) {
        io.aluSrc := "b11".U
      } .otherwise {
        io.aluSrc := "b01".U
      }
      io.regWrite := true.B
      io.aluOpcode := insType.ADD.U
      io.regWriteSrc := RegWriteSrc.ALU.id.U
      io.jumpEnable := true.B
    }
    is(Types.ECALL.id.U) {
      //TODO ECALL AND EBREAK
    }
  }



}

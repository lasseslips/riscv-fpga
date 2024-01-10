package riscv

import chisel3.util._
import chisel3._
import riscv.AluType._

class Alu extends Module {
  val io = IO(new Bundle() {
    val DecEx = Input(new DecEx())
    val ExMem = Output(new ExMem())
    val ExFe = Output(new ExFe())
    val flush = Output(Bool())
    val stall = Input(Bool())
    val forward1 = Input(UInt(2.W))
    val MemAddr = Input(UInt(32.W))
    val WbData = Input(UInt(32.W))
    val forward2 = Input(UInt(2.W))
  })


  val decExReg = RegNext(io.DecEx)
  val flush = WireDefault(Bool(), false.B)
  io.flush := flush
  when(flush) {
    decExReg := Zeroed.DecEx()
  }

  // default value
  val res = Wire(UInt(32.W))
  val op1 = Wire(UInt(32.W))
  val op2 = Wire(UInt(32.W))
  res := DontCare
  op1 := DontCare
  op2 := DontCare
  val aluOpcode = decExReg.aluOpcode

  val rs1 = WireDefault(0.U(32.W))
  val rs2 = WireDefault(0.U(32.W))
  switch(io.forward1) {
    is(ForwardingType.EXMEM.id.U) {
      rs1 := io.MemAddr
    }
    is(ForwardingType.MEMWB.id.U) {
      rs1 := io.WbData
    }
    is(ForwardingType.REGFILE.id.U) {
      rs1 := decExReg.regData1
    }
   }
  switch(io.forward2) {
    is(ForwardingType.EXMEM.id.U) {
      rs2 := io.MemAddr
    }
    is(ForwardingType.MEMWB.id.U) {
      rs2 := io.WbData
    }
    is(ForwardingType.REGFILE.id.U) {
      rs2 := decExReg.regData2
    }
  }

  op1 := Mux(decExReg.aluSrc(1),decExReg.pc,rs1)
  op2 := Mux(decExReg.aluSrc(0),decExReg.imm,rs2)

  //ADD
  switch (aluOpcode) {
    is(ADD.id.U) {
      res := op1 + op2
   }
    is(SUB.id.U) {
      res := op1 - op2
    }
    is(SLL.id.U) {
      res := op1 << op2(4,0)
    }
    is(XOR.id.U) {
      res := op1 ^ op2
    }
    is(SRL.id.U) {
      res := op1 >> op2(4,0)
    }
    is(SRA.id.U) {
      res := (op1.asSInt >> op2(4,0)).asUInt
    }
    is(OR.id.U) {
      res := op1 | op2
    }
    is(AND.id.U) {
      res := op1 & op2
    }
    is(SLT.id.U) {
      res := (op1.asSInt < op2.asSInt).asUInt
    }
    is(SLTU.id.U) {
      res := (op1 < op2).asUInt
    }
  }

  val branch = Module(new Branch())

  branch.io.rs1 := rs1
  branch.io.rs2 := rs2
  branch.io.branchEnable := decExReg.branchEnable
  branch.io.branchType := decExReg.branchType

  val jump = WireDefault(false.B)
  io.ExFe.jump := jump
  jump := decExReg.jumpEnable || (branch.io.branching)
  flush := jump




  io.ExFe.pc := res
  io.ExMem.addr := res
  io.ExMem.pc := decExReg.pc
  io.ExMem.data := rs2

  io.ExMem.regWrite := decExReg.regWrite
  io.ExMem.memWrite := decExReg.memWrite
  io.ExMem.memIns := decExReg.memIns
  io.ExMem.regWrIdx := decExReg.regWrIdx
  io.ExMem.regWriteSrc := decExReg.regWriteSrc

  //stall
  when(io.stall) {
    decExReg := Zeroed.DecEx()
  }
  



}

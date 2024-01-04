package riscv

import chisel3.util._
import chisel3._
import riscv.AluType._

class Alu extends Module {
  val io = IO(new Bundle() {
    val DecEx = Input(new DecEx())
    val ExMem = Output(new ExMem())
    val ExFe = Output(new ExFe())
  })


  // default value
  val res = Wire(UInt(32.W))
  val op1 = Wire(UInt(32.W))
  val op2 = Wire(UInt(32.W))
  res := DontCare
  op1 := DontCare
  op2 := DontCare
  val aluOpcode = io.DecEx.aluOpcode

  op1 := Mux(io.DecEx.aluSrc(0),io.DecEx.pc,io.DecEx.regData1)
  op2 := Mux(io.DecEx.aluSrc(1),io.DecEx.imm,io.DecEx.regData2)

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

  branch.io.rs1 := io.DecEx.regData1
  branch.io.rs2 := io.DecEx.regData2
  branch.io.branchEnable := io.DecEx.branchEnable
  branch.io.branchType := io.DecEx.branchType

  val jump = WireDefault(false.B)
  io.ExFe.jump := jump
  jump := io.DecEx.jumpEnable || (branch.io.branching)




  io.ExFe.pc := res
  io.ExMem.addr := res
  io.ExMem.pc := io.DecEx.pc
  io.ExMem.data := io.DecEx.regData2

  io.ExMem.regWrite := io.DecEx.regWrite
  io.ExMem.memWrite := io.DecEx.memWrite
  io.ExMem.memIns := io.DecEx.memIns
  io.ExMem.regWrIdx := io.DecEx.regWrIdx
  io.ExMem.regWriteSrc := io.DecEx.regWriteSrc


  



}

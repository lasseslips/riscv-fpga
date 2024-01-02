package riscv

import chisel3.util._
import chisel3._
import riscv.AluType._

class Alu extends Module {
  val io = IO(new Bundle() {
    val DecEx = Input(new DecEx())
    val ExMem = Output(new ExMem())
    val control = Input(UInt(5.W))
    val aluSrc = Input(UInt(2.W))
  })


  // default value
  val res = DontCare
  val op1 = DontCare
  val op2 = DontCare
  val control = DontCare

  switch(io.aluSrc) {
    is("b00".U) {
      op1 := io.DecEx.regData1
      op2 := io.DecEx.regData2
    }
    is("b01".U) {
      op1 := io.DecEx.regData1
      op2 := io.DecEx.imm
    }
    is("b11".U) {
      op1 := io.DecEx.pc
      op2 := io.DecEx.imm
    }
    is("b10".U) {
      op1 := io.DecEx.pc
      op2 := io.DecEx.regData2
    }
  }

  //ADD
  switch (control) {
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

  io.ExMem.addr := res
  io.ExMem.pc := io.DecEx.pc
  


}

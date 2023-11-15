package riscv

import chisel3.util._
import chisel3._
import riscv.AluType._

class Alu extends Module {
  val io = IO(new Bundle() {
    val op1 = Input(UInt(32.W))
    val op2 = Input(UInt(32.W))
    val res = Output(UInt(32.W))
    val control = Input(UInt(5.W))
  })


  // default value
  io.res := DontCare

  //ADD
  switch (io.control) {
    is(ADD.id.U) {
      io.res := io.op1 + io.op2
   }
    is(SUB.id.U) {
      io.res := io.op1 - io.op2
    }
    is(SLL.id.U) {
      io.res := io.op1 << io.op2(4,0)
    }
    is(XOR.id.U) {
      io.res := io.op1 ^ io.op2
    }
    is(SRL.id.U) {
      io.res := io.op1 >> io.op2(4,0)
    }
    is(SRA.id.U) {
      io.res := (io.op1.asSInt >> io.op2(4,0)).asUInt
    }
    is(OR.id.U) {
      io.res := io.op1 | io.op2
    }
    is(AND.id.U) {
      io.res := io.op1 & io.op2
    }
    is(SLT.id.U) {
      io.res := (io.op1.asSInt < io.op2.asSInt).asUInt
    }
    is(SLTU.id.U) {
      io.res := (io.op1 < io.op2).asUInt
    }
  }
}

package riscv

import chisel3.util._
import chisel3._

class Alu extends Module {
  val io = IO(new Bundle() {
    val op1 = Input(UInt(32.W))
    val op2 = Input(UInt(32.W))
    val res = Output(UInt(32.W))
    val control = Input(UInt(5.W))
  })
  val Add = 0.U
  val Sub = 1.U
  val Sll = 2.U
  val Xor = 3.U
  val Srl = 4.U
  val Sra = 5.U
  val Or = 6.U
  val And = 7.U
  val Slt = 8.U
  val Sltu = 9.U


  // default value
  io.res := DontCare

  //ADD
  switch (io.control) {
    is(Add) {
      io.res := io.op1 + io.op2
   }
    is(Sub) {
      io.res := io.op1 - io.op2
    }
    is(Sll) {
      io.res := io.op1 << io.op2(4,0)
    }
    is(Xor) {
      io.res := io.op1 ^ io.op2
    }
    is(Srl) {
      io.res := io.op1 >> io.op2(4,0)
    }
    is(Sra) {
      io.res := (io.op1.asSInt >> io.op2(4,0)).asUInt
    }
    is(Or) {
      io.res := io.op1 | io.op2
    }
    is(And) {
      io.res := io.op1 & io.op2
    }
    is(Slt) {
      io.res := (io.op1.asSInt < io.op2.asSInt).asUInt
    }
    is(Sltu) {
      io.res := (io.op1 < io.op2).asUInt
    }
  }
}

package riscv

import Chisel.{is, switch}
import chisel3._
class Alu extends Module {
  val io = IO(new Bundle() {
    val op1 = Input(UInt(32.W))
    val op2 = Input(UInt(32.W))
    val res = Output(UInt(32.W))
    val control = Input(UInt(5.W))
  })


  //ADD
  switch(io.control) {
    is("b000".U) {
      io.res := io.op1 + io.op2
   }
    is("b001".U) {
      io.res := io.op1 - io.op2
    }
    is("b010".U) {
      io.res := io.op1 << io.op2
    }
    is("b011".U) {
      io.res := io.op1 ^ io.op2
    }
  }

}

package riscv

import chisel3._
import chisel3.util._
class Registers extends Module {
  val io = IO(new Bundle() {
    val dataIn = Input(UInt(32.W))
    val wrIdx = Input(UInt(5.W))
    val rs1Idx = Input(UInt(5.W))
    val rs2Idx = Input(UInt(5.W))
    val write = Input(Bool())
    val reg1 = Output(UInt(32.W))
    val reg2 = Output(UInt(32.W))
  })

  io.reg1 := DontCare
  io.reg2 := DontCare

  val registers = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  registers(0) := 0.U

  io.reg1 := registers(io.rs1Idx)
  io.reg2 := registers(io.rs2Idx)

  when(io.write) {
    registers(io.wrIdx) := io.dataIn
  }

}

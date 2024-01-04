package riscv

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
class InstructionMemory(code: Array[Int]) extends Module {
  val io = IO(new Bundle() {
    val FeDec = Output(new FeDec())
    val ExFe = Input(new ExFe())
    val halt = Input(Bool())
  })


  io.FeDec.instruction := DontCare
  val pc = RegInit(0.U(32.W))
  val halt = Wire(Bool())
  halt := io.halt

  val nextPc = WireDefault(0.U(32.W))
  val mem = VecInit(code.map(_.S(32.W).asUInt))
  when(!halt) {
    nextPc := Mux(io.ExFe.jump, io.ExFe.pc + 8.U, pc + 4.U)
    io.FeDec.instruction := mem((nextPc - 4.U) / 4.U)
  } .otherwise {
    io.FeDec.instruction := "h00000013".U // NOP
  }
  pc := nextPc


  // 262kB
  //val mem = SyncReadMem(Math.pow(2,16).toInt,UInt(32.W))
  //loadMemoryFromFile(mem,pathToBin)
  //io.FeDec.instruction := mem.read(((nextPc - 4.U) / 4.U), true.B)


  io.FeDec.pc := pc
}



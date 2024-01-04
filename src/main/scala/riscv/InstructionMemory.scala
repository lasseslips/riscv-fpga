package riscv

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
class InstructionMemory(pathToBin: String = "") extends Module {
  val io = IO(new Bundle() {
    val FeDec = Output(new FeDec())
    val ExFe = Input(new ExFe())
  })


  val pc = RegInit(0.U(32.W))

  val nextPc = WireDefault(0.U(32.W))
  nextPc := Mux(io.ExFe.jump, io.ExFe.pc + 4.U, pc + 4.U)
  pc := nextPc


  io.FeDec.instruction := DontCare
  // 262kB
  val mem = SyncReadMem(Math.pow(2,16).toInt,UInt(32.W))
  loadMemoryFromFile(mem,pathToBin)
  io.FeDec.instruction := mem.read(((nextPc - 4.U) / 4.U), true.B)

  io.FeDec.pc := pc
}



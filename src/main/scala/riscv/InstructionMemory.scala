package riscv

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
class InstructionMemory(pathToBin: String = "") extends Module {
  val io = IO(new Bundle() {
    val FeDec = Output(new FeDec())
    val pc = Input(UInt(16.W))
  })


  io.FeDec.instruction := DontCare
  // 262kB
  val mem = SyncReadMem(Math.pow(2,16).toInt,UInt(32.W))
  loadMemoryFromFile(mem,pathToBin)
  io.FeDec.instruction := mem.read((io.pc / 4.U), true.B)
}



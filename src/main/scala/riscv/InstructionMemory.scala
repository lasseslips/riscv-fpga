package riscv

import chisel3._
import chisel3.util._
class InstructionMemory extends Module {
  val io = IO(new Bundle() {
    val addr = Input(UInt(16.W))
    val dataOut = Output(UInt(32.W))
    val dataIn = Input(UInt(32.W))
    val write = Input(Bool())
  })


  io.dataOut := DontCare
  // 262kB
  val mem = SyncReadMem(Math.pow(2,16).toInt,UInt(32.W))

  when(io.write) {
    mem.write(io.addr,io.dataIn)
  } .otherwise {
    io.dataOut := mem.read(io.addr, true.B)
  }
}



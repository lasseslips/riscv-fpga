package riscv

import chisel3._
import chisel3.util._
class DataMemory extends Module {
  val io = IO(new Bundle() {
    val addr = Input(UInt(20.W))
    val enable = Input(Bool())
    val write = Input(Bool())
    val dataIn = Input(UInt(8.W))
    val dataOut = Output(UInt(8.W))
  })


  // 1,048,576 x 8 bit = 8Mb
  val mem = SyncReadMem(Math.pow(2, 20).toInt, UInt(8.W))
  io.dataOut := DontCare
  when(io.enable) {
    val rdwrPort = mem(io.addr)
    when (io.write) {
      rdwrPort := io.dataIn
    } .otherwise {
      io.dataOut := rdwrPort
    }
  }
  io.dataOut := mem.read(io.addr, true.B)
}

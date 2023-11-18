package riscv

import chisel3._
import chisel3.util._
class DataMemory extends Module {
  val io = IO(new Bundle() {
    val addr = Input(UInt(20.W))
    val write = Input(Bool())
    val dataIn = Input(Vec(4,UInt(8.W)))
    val dataOut = Output(Vec(4,UInt(8.W)))
    val mask = Input(Vec(4,Bool()))
  })



  // 1,048,576 x 32 bit = 32Mb
  val mem = SyncReadMem(Math.pow(2, 20).toInt, Vec(4, UInt(8.W)))

  io.dataOut := DontCare
  when(io.write) {
    mem.write(io.addr,io.dataIn,io.mask)
  }
  val data = mem.read(io.addr)

  for (i <- 0 until 4) {
    when(io.mask(i)) {
      io.dataOut(i) := data(i)
    } .otherwise {
      io.dataOut(i) := 0.U
    }
  }

}

package riscv

import chisel3._
import chisel3.util._
class WriteBack extends Module {
  val io = IO(new Bundle() {
    val MemWb = Input(new MemWb())
    val regSrc = Input(UInt(2.W))
    val wrData = Output(UInt(32.W))
  })

  switch(io.regSrc) {
    is(RegWriteSrc.ALU.id.U) {
      io.wrData := io.MemWb.alu
    }
    is(RegWriteSrc.MEMORY.id.U) {
      io.wrData := io.MemWb.mem
    }
    is(RegWriteSrc.PC.id.U) {
      io.wrData := io.MemWb.pc + 4.U
    }
  }

}

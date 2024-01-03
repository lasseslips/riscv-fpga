package riscv

import chisel3._
import chisel3.util._
class WriteBack extends Module {
  val io = IO(new Bundle() {
    val MemWb = Input(new MemWb())
    val WbDec = Output(new WbDec())
  })
  io.WbDec.wrData := DontCare

  switch(io.MemWb.regWriteSrc) {
    is(RegWriteSrc.ALU.id.U) {
      io.WbDec.wrData := io.MemWb.alu
    }
    is(RegWriteSrc.MEMORY.id.U) {
      io.WbDec.wrData := io.MemWb.mem
    }
    is(RegWriteSrc.PC.id.U) {
      io.WbDec.wrData := io.MemWb.pc + 4.U
    }
  }

  io.WbDec.regWrIdx := io.MemWb.regWrIdx
  io.WbDec.regWrite := io.MemWb.regWrite
}

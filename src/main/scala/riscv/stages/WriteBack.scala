package riscv.stages

import chisel3._
import chisel3.util._
import riscv.lib.{MemWb, RegWriteSrc, WbDec}
class WriteBack extends Module {
  val io = IO(new Bundle() {
    val MemWb = Input(new MemWb())
    val WbDec = Output(new WbDec())
  })

  val memWbReg = RegNext(io.MemWb)

  io.WbDec.wrData := DontCare

  switch(memWbReg.regWriteSrc) {
    is(RegWriteSrc.ALU.id.U) {
      io.WbDec.wrData := memWbReg.alu
    }
    is(RegWriteSrc.MEMORY.id.U) {
      io.WbDec.wrData := memWbReg.mem
    }
    is(RegWriteSrc.PC.id.U) {
      io.WbDec.wrData := memWbReg.pc + 4.U
    }
  }

  io.WbDec.regWrIdx := memWbReg.regWrIdx
  io.WbDec.regWrite := memWbReg.regWrite
}

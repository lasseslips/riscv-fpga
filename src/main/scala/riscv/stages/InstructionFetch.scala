package riscv.stages

import chisel3._
import riscv.lib.{ExFe, FeDec}

class InstructionFetch(code: Array[Int]) extends Module {
  val io = IO(new Bundle() {
    val FeDec = Output(new FeDec())
    val ExFe = Input(new ExFe())
    val halt = Input(Bool())
    val stall = Input(Bool())
  })


  io.FeDec.instruction := DontCare
  val pc = RegInit(0.U(32.W))

  val mem = VecInit(code.map(_.S(32.W).asUInt))
  when(!io.halt) {
    when(io.stall) {
      pc := Mux(io.ExFe.jump, io.ExFe.pc, pc)
    } .otherwise {
      pc := Mux(io.ExFe.jump, io.ExFe.pc, pc + 4.U)
    }
    io.FeDec.instruction := mem(pc / 4.U)
  } .otherwise {
    io.FeDec.instruction := "h00000013".U // NOP
  }

  io.FeDec.pc := pc

}



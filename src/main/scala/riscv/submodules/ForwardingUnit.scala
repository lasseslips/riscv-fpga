package riscv.submodules

import chisel3._
import chisel3.util._
import riscv.lib.{ForwardingType, RegWriteSrc}

class ForwardingUnit extends Module {
  val io = IO(new Bundle() {
    val ExwrIdx = Input(UInt(5.W))
    val ExRegWrite = Input(Bool())
    val MemRegWrite = Input(Bool())
    val MemwrIdx = Input(UInt(5.W))
    val rs1 = Input(UInt(5.W))
    val rs2 = Input(UInt(5.W))
    val stall = Output(Bool())
    val forward1 = Output(UInt(3.W))
    val forward2 = Output(UInt(3.W))
    val regWriteSrc = Input(UInt(2.W))
  })
  val forward1Reg = RegInit(0.U(3.W))
  val forward2Reg = RegInit(0.U(3.W))
  io.forward1 := forward1Reg
  io.forward2 := forward2Reg

  io.stall := false.B
  /*
  when(io.ExRegWrite && (io.ExwrIdx === io.rs1 || io.ExwrIdx === io.rs2) && (io.ExwrIdx =/= 0.U)) {
    io.stall := true.B
  } .elsewhen(io.MemRegWrite && (io.MemwrIdx === io.rs1 || io.MemwrIdx === io.rs2) && (io.MemwrIdx =/= 0.U)) {
    io.stall := true.B
  } .otherwise {
    io.stall := false.B
  }
   */

  //FORWARDING
  when(io.ExRegWrite && (io.ExwrIdx === io.rs1) && (io.ExwrIdx =/= 0.U)) {
    switch(io.regWriteSrc) {
      is(RegWriteSrc.ALU.id.U) {
        forward1Reg := ForwardingType.EXMEM.id.U
      }
      is(RegWriteSrc.MEMORY.id.U) {
        forward1Reg := ForwardingType.MEMDATA.id.U
      }
      is(RegWriteSrc.PC.id.U) {
        forward1Reg := ForwardingType.PC.id.U
      }
    }
  } .elsewhen(io.MemRegWrite && (io.MemwrIdx === io.rs1) && (io.MemwrIdx =/= 0.U)) {
    forward1Reg := ForwardingType.MEMWB.id.U
  } .otherwise {
    forward1Reg := ForwardingType.REGFILE.id.U
  }
  when(io.ExRegWrite && (io.ExwrIdx === io.rs2) && (io.ExwrIdx =/= 0.U)) {
    switch(io.regWriteSrc) {
      is(RegWriteSrc.ALU.id.U) {
        forward2Reg := ForwardingType.EXMEM.id.U
      }
      is(RegWriteSrc.MEMORY.id.U) {
        forward2Reg := ForwardingType.MEMDATA.id.U
      }
      is(RegWriteSrc.PC.id.U) {
        forward2Reg := ForwardingType.PC.id.U
      }
    }
  } .elsewhen(io.MemRegWrite && (io.MemwrIdx === io.rs2) && (io.MemwrIdx =/= 0.U)) {
    forward2Reg := ForwardingType.MEMWB.id.U
  } .otherwise {
    forward2Reg := ForwardingType.REGFILE.id.U
  }





}

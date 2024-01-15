package riscv

import chisel3._
import chisel3.util._
class HazardDetection extends Module {
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
  val ExwrIdx = Wire(UInt(5.W))
  val ExRegWrite = Wire(Bool())
  val MemRegWrite = Wire(Bool())
  val MemwrIdx = Wire(UInt(5.W))
  val rs1 = Wire(UInt(5.W))
  val rs2 = Wire(UInt(5.W))
  ExwrIdx := io.ExwrIdx
  ExRegWrite := io.ExRegWrite
  MemRegWrite := io.MemRegWrite
  MemwrIdx := io.MemwrIdx
  rs1 := io.rs1
  rs2 := io.rs2
  val forward1Reg = RegInit(0.U(3.W))
  val forward2Reg = RegInit(0.U(3.W))
  io.forward1 := forward1Reg
  io.forward2 := forward2Reg

  io.stall := false.B
  /*
  when(ExRegWrite && (ExwrIdx === rs1 || ExwrIdx === rs2) && (ExwrIdx =/= 0.U)) {
    io.stall := true.B
  } .elsewhen(MemRegWrite && (MemwrIdx === rs1 || MemwrIdx === rs2) && (MemwrIdx =/= 0.U)) {
    io.stall := true.B
  } .otherwise {
    io.stall := false.B
  }
   */

  //FORWARDING
  when(ExRegWrite && (ExwrIdx === rs1) && (ExwrIdx =/= 0.U)) {
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
  } .elsewhen(MemRegWrite && (MemwrIdx === rs1) && (MemwrIdx =/= 0.U)) {
    forward1Reg := ForwardingType.MEMWB.id.U
  } .otherwise {
    forward1Reg := ForwardingType.REGFILE.id.U
  }
  when(ExRegWrite && (ExwrIdx === rs2) && (ExwrIdx =/= 0.U)) {
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
  } .elsewhen(MemRegWrite && (MemwrIdx === rs2) && (MemwrIdx =/= 0.U)) {
    forward2Reg := ForwardingType.MEMWB.id.U
  } .otherwise {
    forward2Reg := ForwardingType.REGFILE.id.U
  }





}

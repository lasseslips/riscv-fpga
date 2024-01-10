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

  when(ExRegWrite && (ExwrIdx === rs1 || ExwrIdx === rs2)) {
    io.stall := true.B
  } .elsewhen(MemRegWrite && (MemwrIdx === rs1 || MemwrIdx === rs2)) {
    io.stall := true.B
  } .otherwise {
    io.stall := false.B
  }





}
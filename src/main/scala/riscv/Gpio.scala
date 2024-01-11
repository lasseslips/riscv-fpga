package riscv

import chisel3._
import chisel3.util._

class Gpio extends Module {
  val io = IO(new Bundle() {
    val MemGpio = Input(new MemGpio())
    val outputPins = Output(UInt(32.W))
    val inputPins = Input(UInt(32.W))
  })

  val controlReg = RegInit(0.U(32.W))
  val dataReg = RegInit(0.U(32.W))
  val addr = Wire(UInt(32.W))
  addr := io.MemGpio.addr
  val data = Wire(UInt(32.W))
  data := io.MemGpio.data

  val outputPins = WireDefault(0.U(32.W))
  io.outputPins := outputPins
  controlReg := "hffffffff".U

  //io starts at address 0x4000.0000
  when(addr(30) === 1.U && io.MemGpio.memWrite) {
    dataReg := data
  }
  
  outputPins := controlReg & dataReg







}

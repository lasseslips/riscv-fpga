package peripherals

import chisel3._
import chisel3.util._
import riscv.MemGpio

class Gpio extends Module {
  val io = IO(new Bundle() {
    val MemGpio = Input(new MemGpio())
    val ledPins = Output(UInt(32.W))
    val sevenSegPins = Output(Vec(8, UInt(7.W)))
    val inputPins = Input(UInt(32.W))
    val lcdPins = Output(UInt(13.W))
  })

  val ledReg = RegInit(0.U(32.W))
  val segReg = RegInit(0.U(32.W))
  val lcdReg = RegInit(0.U(13.W))
  val addr = Wire(UInt(32.W))
  addr := io.MemGpio.addr
  val data = Wire(UInt(32.W))
  data := io.MemGpio.data

  io.ledPins := ledReg
  io.lcdPins := lcdReg

  //led and sevenseg starts at address 0x4000.0000
  when((addr >> (7 * 4)) === 4.U && io.MemGpio.memWrite) {
    when(addr(5)) {
      segReg := data
    } .otherwise {
      ledReg := data
    }
  }
  //lcd starts at address 0x5000.0000
  when((addr >> (7 * 4)) === 5.U && io.MemGpio.memWrite) {
    lcdReg := data(12,0)
  }

  for (i <- 0 until 8) yield {
    val sevenSegDecoder = Module(new SevenSegDecoder())
    val swData = Wire(UInt(4.W))
    swData := segReg(4*i+3,4*i)
    sevenSegDecoder.io.sw := swData
    io.sevenSegPins(i) := sevenSegDecoder.io.seg
  }
}

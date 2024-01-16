package riscv

import chisel3._
import chisel3.util._
import peripherals.Gpio
class DataPath(pathToBin: String = "") extends Module {
  val io = IO(new Bundle() {
    val ledOut = Output(UInt(32.W))
    val sevenSegPins = Output(Vec(8, UInt(7.W)))
    val lcdPins = Output(UInt(13.W))


    //DEBUG
    val registers = Output(Vec(32, UInt(32.W)))
    //val reg12_0 = Output(UInt(1.W))
    //val test = Output(Bool())


    //DONT DELETE
    val ins = Output(UInt(32.W))

  })
  //DEBUG BLINKING LED
  /*val CNT_MAX = (40000000 / 2 - 1).U
  
  val cntReg = RegInit(0.U(32.W))
  val blkReg = RegInit(0.U(1.W))

  cntReg := cntReg + 1.U
  when(cntReg === CNT_MAX) {
    cntReg := 0.U
    blkReg := ~blkReg
  }
  io.test := blkReg
  */

  //Util.convertBinToHex(pathToBin)
  val code = Util.readBin(pathToBin)

  //Module loading
  val instructionMemory = Module(new InstructionMemory(code))
  val decode = Module(new Decode())
  val alu = Module(new Alu())
  val dataMemory = Module(new DataMemory())
  val writeBack = Module(new WriteBack())
  val gpio = Module(new Gpio())

  //Connections
  instructionMemory.io.FeDec <> decode.io.FeDec
  instructionMemory.io.ExFe <> alu.io.ExFe
  decode.io.DecEx <> alu.io.DecEx


  gpio.io.inputPins := DontCare
  gpio.io.MemGpio := DontCare
  io.ledOut := gpio.io.ledPins
  io.sevenSegPins := gpio.io.sevenSegPins
  io.lcdPins := gpio.io.lcdPins


  when(alu.io.ExMem.addr(30) === 1.U) {
    gpio.io.MemGpio.addr := alu.io.ExMem.addr
    gpio.io.MemGpio.data := alu.io.ExMem.data
    gpio.io.MemGpio.memWrite := alu.io.ExMem.memWrite
    }
    alu.io.ExMem <> dataMemory.io.ExMem


  alu.io.MemAddr := dataMemory.io.MemAddr
  alu.io.WbData := writeBack.io.WbDec.wrData
  alu.io.memData := dataMemory.io.MemWb.mem
  alu.io.memPc := dataMemory.io.MemWb.pc
  dataMemory.io.MemWb <> writeBack.io.MemWb
  writeBack.io.WbDec <> decode.io.WbDec
  decode.io.flush := alu.io.flush

  instructionMemory.io.halt := false.B
  val haltReg = RegInit(Bool(),false.B)
  when(decode.io.halt || haltReg) {
    haltReg := true.B
    instructionMemory.io.halt := true.B
  }


  //Hazard detection
  val hazardDetection = Module(new ForwardingUnit())
  hazardDetection.io.ExwrIdx := alu.io.ExMem.regWrIdx
  hazardDetection.io.MemwrIdx := dataMemory.io.MemWb.regWrIdx
  hazardDetection.io.ExRegWrite := alu.io.ExMem.regWrite
  hazardDetection.io.MemRegWrite := dataMemory.io.MemWb.regWrite
  hazardDetection.io.rs1 := decode.io.rs1
  hazardDetection.io.rs2 := decode.io.rs2
  hazardDetection.io.regWriteSrc := alu.io.ExMem.regWriteSrc
  instructionMemory.io.stall := hazardDetection.io.stall
  decode.io.stall := hazardDetection.io.stall
  alu.io.stall := hazardDetection.io.stall
  alu.io.forward1 := hazardDetection.io.forward1
  alu.io.forward2 := hazardDetection.io.forward2

  //DONT DELETE
  io.ins := instructionMemory.io.FeDec.instruction


  //Debug
  
  io.registers := decode.io.registers
  /*
  val slice = Wire(UInt(32.W))
  slice := decode.io.registers(12)
  io.reg12_0 := slice(0)
  */


}

object Main extends App {
  println("Generating RISC-V verilog")
  emitVerilog(new DataPath("bin/print"), Array("--target-dir", "generated"))
}

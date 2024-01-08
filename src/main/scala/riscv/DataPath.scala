package riscv

import chisel3._
import chisel3.util._
class DataPath(pathToBin: String = "") extends Module {
  val io = IO(new Bundle() {
    //DEBUG
    //val registers = Output(Vec(32, UInt(32.W)))
    val reg12_0 = Output(UInt(1.W))
    val test = Output(Bool())


    //DONT DELETE
    val ins = Output(UInt(32.W))

  })
  val CNT_MAX = (1000000 / 2 - 1).U
  
  val cntReg = RegInit(0.U(32.W))
  val blkReg = RegInit(0.U(1.W))

  cntReg := cntReg + 1.U
  when(cntReg === CNT_MAX) {
    cntReg := 0.U
    blkReg := ~blkReg
  }
  io.test := blkReg

  //Util.convertBinToHex(pathToBin)
  val code = Util.readBin(pathToBin)

  //Module loading
  val instructionMemory = Module(new InstructionMemory(code))
  val decode = Module(new Decode())
  val alu = Module(new Alu())
  val dataMemory = Module(new DataMemory())
  val writeBack = Module(new WriteBack())

  //Connections
  instructionMemory.io.FeDec <> decode.io.FeDec
  instructionMemory.io.ExFe <> alu.io.ExFe
  decode.io.DecEx <> alu.io.DecEx
  alu.io.ExMem <> dataMemory.io.ExMem
  dataMemory.io.MemWb <> writeBack.io.MemWb
  writeBack.io.WbDec <> decode.io.WbDec
  decode.io.flush := alu.io.flush

  instructionMemory.io.halt := false.B
  val haltReg = RegInit(Bool(),false.B)
  when(decode.io.halt || haltReg) {
    haltReg := true.B
    instructionMemory.io.halt := true.B
  }


  //DONT DELETE
  io.ins := instructionMemory.io.FeDec.instruction


  //Debug
  //io.registers := decode.io.registers
  val slice = Wire(UInt(32.W))
  slice := decode.io.registers(12)
  io.reg12_0 := slice(0)


}

object Main extends App {
  println("Generating RISC-V verilog")
  emitVerilog(new DataPath("bin/blinkingLed"), Array("--target-dir", "generated"))
}

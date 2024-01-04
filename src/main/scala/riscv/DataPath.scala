package riscv

import chisel3._
import chisel3.util._
class DataPath(pathToBin: String = "") extends Module {
  val io = IO(new Bundle() {
    //DEBUG
    val registers = Output(Vec(32, UInt(32.W)))


    //DONT DELETE
    val ins = Output(UInt(32.W))

  })

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
  io.registers := decode.io.registers

}

object Main extends App {
  println("Generating RISC-V verilog")
  emitVerilog(new DataPath("bin/addlarge"), Array("--target-dir", "generated"))
}

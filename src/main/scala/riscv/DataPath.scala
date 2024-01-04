package riscv

import chisel3._
import chisel3.util._
class DataPath(pathToBin: String = "") extends Module {
  val io = IO(new Bundle() {
    //DEBUG


    //DONT DELETE
    val ins = Output(UInt(32.W))

  })

  //Util.convertBinToHex(pathToBin)

  //Module loading
  val instructionMemory = Module(new InstructionMemory(pathToBin + ".hex"))
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


  //DONT DELETE
  io.ins := instructionMemory.io.FeDec.instruction


}

object Main extends App {
  println("Generating RISC-V verilog")
  emitVerilog(new DataPath("bin/addlarge"), Array("--target-dir", "generated"))
}

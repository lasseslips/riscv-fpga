package riscv

import chisel3._
import chisel3.util._
class DataPath(pathToBin: String = "") extends Module {
  val io = IO(new Bundle() {
    //DEBUG
    val writeBack = Output(UInt(32.W))
    //val instructionMem = Output(UInt(32.W))
    val instructionDataIn = Input(UInt(32.W))
    val instructionWrite = Input(Bool())
    val reg1 = Output(UInt(32.W))
    val reg2 = Output(UInt(32.W))
    val rs1Idx = Input(UInt(5.W))
  })

  Util.convertBinToHex(pathToBin)

  io.writeBack := DontCare

  //Module loading
  val insMem = Module(new InstructionMemory(pathToBin + ".hex"))
  val decode = Module(new Decode)
  val control = Module(new Control)
  val alu = Module(new Alu)
  val branchControl = Module(new Branch)
  val dataMemory = Module(new DataMemory)
  val writeback = Module(new WriteBack)



  //Init
  val aluRes = WireDefault(UInt(32.W), DontCare)
  val regWriteSrc = WireDefault(UInt(2.W),DontCare)
  regWriteSrc := control.io.regWriteSrc
  val aluSrc = WireDefault(UInt(2.W),DontCare)
  aluSrc := control.io.aluSrc

  //Jump
  val jmp = WireInit(false.B)

  //PC
  val pc = RegInit(0.U(32.W))
  pc := Mux(jmp,aluRes,pc + 4.U)


  insMem.io.FeDec <> decode.io.FeDec
  decode.io.DecEx <> alu.io.DecEx
  alu.io.ExMem <> dataMemory.io.ExMem
  dataMemory.io.MemWb <> writeback.io.MemWb

  //InstructionMemory
  insMem.io.pc := pc

  //io.instructionMem := decode.io.instructiontest
  //io.instructionMem := insMem.io.dataOut

  //Decode


  //ALU

  //BranchControl
  branchControl.io.branchType := control.io.branchType
  branchControl.io.branchEnable := control.io.branchEnable

  //jump
  jmp := branchControl.io.branching || control.io.jumpEnable

  //DataMemory

}

object Main extends App {
  println("Generating RISC-V verilog")
  emitVerilog(new DataPath("bin/addpos"), Array("--target-dir", "generated"))
}

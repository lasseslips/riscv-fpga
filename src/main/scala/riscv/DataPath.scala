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
  val registerFile = Module(new Registers)
  val alu = Module(new Alu)
  val branchControl = Module(new Branch)
  val dataMemory = Module(new DataMemory)

  io.reg1 := registerFile.io.reg1
  io.reg2 := registerFile.io.reg2
  registerFile.io.rs1Idx := io.rs1Idx


  //Init
  val aluRes = WireDefault(UInt(32.W), DontCare)
  aluRes := alu.io.res
  val regWriteSrc = WireDefault(UInt(2.W),DontCare)
  regWriteSrc := control.io.regWriteSrc
  val aluSrc = WireDefault(UInt(2.W),DontCare)
  aluSrc := control.io.aluSrc

  //Jump
  val jmp = WireInit(false.B)

  //PC
  val pc = RegInit(0.U(32.W))
  pc := Mux(jmp,aluRes,pc + 4.U)



  //InstructionMemory
  insMem.io.addr := pc
  insMem.io.dataIn := io.instructionDataIn
  insMem.io.write := io.instructionWrite

  //io.instructionMem := decode.io.instructiontest
  //io.instructionMem := insMem.io.dataOut

  //Decode
  decode.io.instruction := insMem.io.dataOut

  //Control
  control.io.insType := decode.io.insType
  control.io.types := decode.io.types

  //registerFile
  registerFile.io.wrIdx := decode.io.wrIdx
  registerFile.io.rs1Idx := decode.io.rs1Idx
  registerFile.io.rs2Idx := decode.io.rs2Idx
  registerFile.io.write := control.io.regWrite
  registerFile.io.dataIn := DontCare

  switch(regWriteSrc) {
    is(RegWriteSrc.ALU.id.U) {
      registerFile.io.dataIn := aluRes
      io.writeBack := aluRes
    }
    is(RegWriteSrc.MEMORY.id.U) {
      registerFile.io.dataIn := dataMemory.io.dataOut
      io.writeBack := dataMemory.io.dataOut
    }
    is(RegWriteSrc.PC.id.U) {
      registerFile.io.dataIn := pc + 4.U
      io.writeBack := pc + 4.U
    }
  }

  //ALU
  alu.io.control := control.io.aluOpcode
  alu.io.op1 := Mux(aluSrc(1),pc,registerFile.io.reg1)
  alu.io.op2 := Mux(aluSrc(0),decode.io.imm,registerFile.io.reg2)

  //BranchControl
  branchControl.io.rs1 := registerFile.io.reg1
  branchControl.io.rs2 := registerFile.io.reg2
  branchControl.io.branchType := control.io.branchType
  branchControl.io.branchEnable := control.io.branchEnable

  //jump
  jmp := branchControl.io.branching || control.io.jumpEnable

  //DataMemory
  dataMemory.io.addr := aluRes
  dataMemory.io.dataIn := registerFile.io.reg2
  dataMemory.io.insType := control.io.memIns
  dataMemory.io.write := control.io.memWrite

}

object Main extends App {
  println("Generating RISC-V verilog")
  emitVerilog(new DataPath("bin/addpos"), Array("--target-dir", "generated"))
}

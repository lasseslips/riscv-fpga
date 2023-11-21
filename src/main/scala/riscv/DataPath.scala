package riscv

import chisel3._
import chisel3.util._
class DataPath extends Module {
  val io = IO(new Bundle() {
    //DEBUG
    val instruction = Input(UInt(32.W))
    val writeBack = Output(UInt(32.W))
  })
  io.writeBack := DontCare

  //Module loading
  val instructionMemory = Module(new InstructionMemory)
  val decode = Module(new Decode)
  val control = Module(new Control)
  val registerFile = Module(new Registers)
  val alu = Module(new Alu)
  val branchControl = Module(new Branch)
  val dataMemory = Module(new DataMemory)



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
  instructionMemory.io.addr := pc
  instructionMemory.io.dataIn := DontCare
  instructionMemory.io.write := DontCare

  //Decode
  //decode.io.instruction := instructionMemory.io.dataOut
  decode.io.instruction := io.instruction

  //Control
  control.io.insType := decode.io.insType
  control.io.types := decode.io.types

  //registerFile
  registerFile.io.wrIdx := decode.io.wrIdx
  registerFile.io.rs1Idx := decode.io.rs1Idx
  registerFile.io.rs2Idx := decode.io.rs2Idx
  registerFile.io.write := control.io.regWrite
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
  registerFile.io.dataIn := DontCare

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
  emitVerilog(new DataPath, Array("--target-dir", "generated"))
}

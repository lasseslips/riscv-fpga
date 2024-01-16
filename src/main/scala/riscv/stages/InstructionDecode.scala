package riscv.stages

import chisel3._
import chisel3.util._
import riscv.submodules.Control
import riscv._
import riscv.lib.{AluFunct3, AluImmFunct3, AluType, DecEx, FeDec, Opcode, Types, WbDec, insType}

class InstructionDecode extends Module {
  val io = IO(new Bundle() {
    val FeDec = Input(new FeDec())
    val DecEx = Output(new DecEx())
    val WbDec = Input(new WbDec())
    val halt = Output(Bool())
    val flush = Input(Bool())
    //for hazard detection
    val rs1 = Output(UInt(5.W))
    val rs2 = Output(UInt(5.W))
    val stall = Input(Bool())
    //Debug
    val opcode = Output(UInt(6.W))
    val types = Output(UInt(5.W))
    val rs1Idx = Output(UInt(5.W))
    val rs2Idx = Output(UInt(5.W))

    val registers = Output(Vec(32,UInt(32.W)))

  })

  val instruction = io.FeDec.instruction
  val instructionReg = RegInit("h13".U(32.W))
  val pcReg = RegInit(0.U(32.W))
  when(!io.stall) {
    instructionReg := instruction
    pcReg := io.FeDec.pc
  }
  when(io.flush) {
    instructionReg := "h13".U(32.W)
  }


  val feDecReg = RegNext(io.FeDec)

  val insOpcode = instructionReg(6, 0)

  val funct3 = instructionReg(14, 12)
  val funct7 = instructionReg(31, 25)
  val rs1Idx = instructionReg(19, 15)
  val rs2Idx = instructionReg(24, 20)
  val wrIdx = instructionReg(11, 7)
  val imm = Wire(UInt(32.W))
  val types = Wire(UInt(6.W))
  val opcode = Wire(UInt(6.W))
  types := DontCare
  opcode := DontCare
  imm := DontCare
  io.rs1 := rs1Idx
  io.rs2 := rs2Idx
  io.rs1Idx := rs1Idx
  io.rs2Idx := rs2Idx
  io.opcode := opcode
  io.types := types


  switch(insOpcode) {
    is(Opcode.Alu.U) {
      types := Types.R.id.U
      switch(funct3) {
        is(AluFunct3.ADD_SUB.U) {
          when(funct7 === "h20".U) {
            opcode := AluType.SUB.id.U
          }.otherwise {
            opcode := AluType.ADD.id.U
          }
        }
        is(AluFunct3.SLL.U) {
          opcode := AluType.SLL.id.U
        }
        is(AluFunct3.SLT.U) {
          opcode := AluType.SLT.id.U
        }
        is(AluFunct3.SLTU.U) {
          opcode := AluType.SLTU.id.U
        }
        is(AluFunct3.XOR.U) {
          opcode := AluType.XOR.id.U
        }
        is(AluFunct3.SRL_SRA.U) {
          when(funct7 === "h20".U) {
            opcode := AluType.SRA.id.U
          }.otherwise {
            opcode := AluType.SRL.id.U
          }
        }
        is(AluFunct3.OR.U) {
          opcode := AluType.OR.id.U
        }
        is(AluFunct3.AND.U) {
          opcode := AluType.AND.id.U
        }
      }
    }
    is(Opcode.AluImm.U) {
      types := Types.I.id.U
      //sign extend imm
      val slice = instructionReg(31, 20)
      val signBit = slice(11)
      imm := Cat(Fill(20, signBit), slice)
      switch(funct3) {
        is(AluImmFunct3.ADDI.U) {
          opcode := AluType.ADD.id.U
        }
        is(AluImmFunct3.SLLI.U) {
          opcode := AluType.SLL.id.U
        }
        is(AluImmFunct3.SLTI.U) {
          opcode := AluType.SLT.id.U
        }
        is(AluImmFunct3.SLTIU.U) {
          opcode := AluType.SLTU.id.U
        }
        is(AluImmFunct3.XORI.U) {
          opcode := AluType.XOR.id.U
        }
        is(AluImmFunct3.SRLI_SRAI.U) {
          imm := instructionReg(24, 20)
          when(funct7 === "h20".U) {
            opcode := AluType.SRA.id.U
          }.otherwise {
            opcode := AluType.SRL.id.U
          }
        }
        is(AluImmFunct3.ORI.U) {
          opcode := AluType.OR.id.U
        }
        is(AluImmFunct3.ANDI.U) {
          opcode := AluType.AND.id.U
        }
      }

    }
    is(Opcode.Branch.U) {
      types := Types.B.id.U
      val imm10_5 = instructionReg(30, 25)
      val imm4_1 = instructionReg(11, 8)
      val imm11 = instructionReg(7)
      val imm12 = instructionReg(31)
      //the fill is for sign extention
      imm := Cat(Fill(20,imm12),imm12, imm11, imm10_5, imm4_1, 0.U)
      opcode := funct3
    }
    is(Opcode.Load.U) {
      types := Types.LOAD.id.U
      val slice = instructionReg(31, 20)
      val signBit = slice(11)
      imm := Cat(Fill(20, signBit), slice)
      opcode := funct3
    }
    is(Opcode.Store.U) {
      types := Types.S.id.U
      val imm4 = instructionReg(11, 7)
      val imm11_5 = instructionReg(31, 25)
      val signBit = imm11_5(6)
      imm := Cat(Fill(20,signBit), imm11_5, imm4)
      opcode := funct3
    }
    is(Opcode.Lui.U) {
      types := Types.U.id.U
      imm := Cat(instructionReg(31, 12), "b0".U(12.W))
      opcode := insType.LUI.U
    }
    is(Opcode.AuiPc.U) {
      types := Types.U.id.U
      imm := Cat(instructionReg(31, 12), "b0".U(12.W))
      opcode := insType.AUIPC.U
    }
    is(Opcode.Jal.U) {
      types := Types.J.id.U
      val imm20 = instructionReg(31)
      val imm19_12 =instructionReg(19, 12)
      val imm11 = instructionReg(20)
      val imm10_1 = instructionReg(30, 21)
      imm := Cat(Fill(12, imm20), imm20, imm19_12, imm11, imm10_1, 0.U)
      opcode := insType.JAL.U
    }
    is(Opcode.JalR.U) {
      types := Types.J.id.U
      //sign extend imm
      val slice = instructionReg(31, 20)
      val signBit = slice(11)
      imm := Cat(Fill(20, signBit), slice)
      opcode := insType.JALR.U
    }
    is(Opcode.Fence.U) {
      //TODO
      opcode := Opcode.Fence.U
    }
    is(Opcode.ECall.U) {
      types := Types.ECALL.id.U
      val isEBreak = instructionReg(20)
      when(isEBreak === 1.U) {
        opcode := insType.EBREAK.U
      }.otherwise {
        opcode := insType.ECALL.U
      }
    }
    is(0.U) {
      //nop
      types := Types.R.id.U
      opcode := AluType.ADD.id.U
    }

  

  }

  //REGISTER FILE
  val reg1 = Wire(UInt(32.W))
  val reg2 = Wire(UInt(32.W))
  val regWrite = Wire(Bool())
  val dataIn = Wire(UInt(32.W))
  val pipelinedWrIdx = Wire(UInt(5.W))
  dataIn := io.WbDec.wrData
  regWrite := io.WbDec.regWrite
  pipelinedWrIdx := io.WbDec.regWrIdx


  val registers = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  registers(0) := 0.U

  reg1 := registers(rs1Idx)
  reg2 := registers(rs2Idx)


  when(regWrite) {
    registers(pipelinedWrIdx) := dataIn
  }
  //may need to be removed, in ripes the data flows through the registers in 0 cycles.
  when(regWrite && (pipelinedWrIdx === rs1Idx) && (pipelinedWrIdx =/= 0.U)) {
    reg1 := dataIn
  }
  when(regWrite && (pipelinedWrIdx === rs2Idx) && (pipelinedWrIdx =/= 0.U)) {
    reg2 := dataIn
  }
  //may not be a good solution
  when(rs1Idx === 0.U) {
    reg1 := 0.U
  }
  when(rs2Idx === 0.U) {
    reg2 := 0.U
  }

  io.DecEx.imm := imm
  io.DecEx.regData1 := reg1
  io.DecEx.regData2 := reg2




  val control = Module(new Control)
  control.io.opcode := opcode
  control.io.types := types
  io.DecEx.regWrite := control.io.regWrite
  io.DecEx.memWrite := control.io.memWrite
  io.DecEx.memIns := control.io.memIns
  io.DecEx.regWriteSrc := control.io.regWriteSrc
  io.DecEx.aluSrc := control.io.aluSrc
  io.DecEx.aluOpcode := control.io.aluOpcode
  io.DecEx.branchEnable := control.io.branchEnable
  io.DecEx.branchType := control.io.branchType
  io.DecEx.jumpEnable := control.io.jumpEnable
  io.halt := control.io.halt

  io.DecEx.regWrIdx := wrIdx
  io.DecEx.pc := pcReg

  io.registers := registers




}

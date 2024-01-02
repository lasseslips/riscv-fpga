package riscv

import chisel3._
import chisel3.util._

class Decode extends Module {
  val io = IO(new Bundle() {
    val instruction = Input(UInt(32.W))
    val wrIdx = Output(UInt(5.W))
    val rs1Idx = Output(UInt(5.W))
    val rs2Idx = Output(UInt(5.W))
    val imm = Output(UInt(32.W))
    val insType = Output(UInt(6.W))
    val types = Output(UInt(5.W))
    val instructiontest = Output(UInt(32.W))
  })

  io.instructiontest := io.instruction

  val insOpcode = io.instruction(6, 0)

  val funct3 = io.instruction(14, 12)
  val funct7 = io.instruction(31, 25)
  io.rs1Idx := io.instruction(19, 15)
  io.rs2Idx := io.instruction(24, 20)
  io.wrIdx := io.instruction(11, 7)
  io.types := DontCare
  io.imm := DontCare
  io.insType := DontCare

  switch(insOpcode) {
    is(Opcode.Alu.U) {
      io.types := Types.R.id.U
      switch(funct3) {
        is(AluFunct3.ADD_SUB.U) {
          when(funct7 === "h20".U) {
            io.insType := AluType.SUB.id.U
          }.otherwise {
            io.insType := AluType.ADD.id.U
          }
        }
        is(AluFunct3.SLL.U) {
          io.insType := AluType.SLL.id.U
        }
        is(AluFunct3.SLT.U) {
          io.insType := AluType.SLT.id.U
        }
        is(AluFunct3.SLTU.U) {
          io.insType := AluType.SLTU.id.U
        }
        is(AluFunct3.XOR.U) {
          io.insType := AluType.XOR.id.U
        }
        is(AluFunct3.SRL_SRA.U) {
          when(funct7 === "h20".U) {
            io.insType := AluType.SRA.id.U
          }.otherwise {
            io.insType := AluType.SRL.id.U
          }
        }
        is(AluFunct3.OR.U) {
          io.insType := AluType.OR.id.U
        }
        is(AluFunct3.AND.U) {
          io.insType := AluType.AND.id.U
        }
      }
    }
    is(Opcode.AluImm.U) {
      io.types := Types.I.id.U
      //sign extend imm
      val slice = io.instruction(31, 20)
      val signBit = slice(11)
      io.imm := Cat(Fill(20, signBit), slice)
      switch(funct3) {
        is(AluImmFunct3.ADDI.U) {
          io.insType := AluType.ADD.id.U
        }
        is(AluImmFunct3.SLLI.U) {
          io.insType := AluType.SLL.id.U
        }
        is(AluImmFunct3.SLTI.U) {
          io.insType := AluType.SLT.id.U
        }
        is(AluImmFunct3.SLTIU.U) {
          io.insType := AluType.SLTU.id.U
        }
        is(AluImmFunct3.XORI.U) {
          io.insType := AluType.XOR.id.U
        }
        is(AluImmFunct3.SRLI_SRAI.U) {
          io.imm := io.instruction(24, 20)
          when(funct7 === "h20".U) {
            io.insType := AluType.SRA.id.U
          }.otherwise {
            io.insType := AluType.SRL.id.U
          }
        }
        is(AluImmFunct3.ORI.U) {
          io.insType := AluType.OR.id.U
        }
        is(AluImmFunct3.ANDI.U) {
          io.insType := AluType.AND.id.U
        }
      }

    }
    is(Opcode.Branch.U) {
      io.types := Types.B.id.U
      val imm10_5 = io.instruction(30, 25)
      val imm4_1 = io.instruction(11, 8)
      val imm11 = io.instruction(7)
      val imm12 = io.instruction(31)
      //the fill is for sign extention
      io.imm := Cat(Fill(20,imm12),imm12, imm11, imm10_5, imm4_1, 0.U)
      io.insType := funct3
    }
    is(Opcode.Load.U) {
      io.types := Types.LOAD.id.U
      val slice = io.instruction(31, 20)
      val signBit = slice(11)
      io.imm := Cat(Fill(20, signBit), slice)
      io.insType := funct3
    }
    is(Opcode.Store.U) {
      io.types := Types.S.id.U
      val imm4 = io.instruction(11, 7)
      val imm11_5 = io.instruction(31, 25)
      val signBit = imm11_5(6)
      io.imm := Cat(Fill(20,signBit), imm11_5, imm4)
      io.insType := funct3
    }
    is(Opcode.Lui.U) {
      io.types := Types.U.id.U
      io.imm := Cat(io.instruction(31, 12), "b0".U(12.W))
      io.insType := insType.LUI.U
    }
    is(Opcode.AuiPc.U) {
      io.types := Types.U.id.U
      io.imm := Cat(io.instruction(31, 12), "b0".U(12.W))
      io.insType := insType.AUIPC.U
    }
    is(Opcode.Jal.U) {
      io.types := Types.J.id.U
      val imm20 = io.instruction(31)
      val imm19_12 = io.instruction(19, 12)
      val imm11 = io.instruction(20)
      val imm10_1 = io.instruction(30, 21)
      io.imm := Cat(Fill(12, imm20), imm20, imm19_12, imm11, imm10_1, 0.U)
      io.insType := insType.JAL.U
    }
    is(Opcode.JalR.U) {
      io.types := Types.J.id.U
      //sign extend imm
      val slice = io.instruction(31, 20)
      val signBit = slice(11)
      io.imm := Cat(Fill(20, signBit), slice)
      io.insType := insType.JALR.U
    }
    is(Opcode.Fence.U) {
      //TODO
      io.insType := insType.FENCE.U
    }
    is(Opcode.ECall.U) {
      io.types := Types.ECALL.id.U
      val isEBreak = io.instruction(20)
      when(isEBreak === 1.U) {
        io.insType := insType.EBREAK.U
      }.otherwise {
        io.insType := insType.ECALL.U
      }
    }
  }

            printf("%d\n", Types.R.id.U)
            printf("%d\n", Types.I.id.U)
            printf("%d\n", Types.S.id.U)
            printf("%d\n", Types.B.id.U)
            printf("%d\n", Types.U.id.U)
            printf("%d\n", Types.J.id.U)
            printf("%d\n", Types.LOAD.id.U)
            printf("%d\n", Types.ECALL.id.U)
            printf("\n")

}

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
  })
  val insOpcode = io.instruction(6,0)

  val funct3 = io.instruction(14,12)
  val funct7 = io.instruction(31,25)
  io.rs1Idx := io.instruction(19,15)
  io.rs2Idx := io.instruction(24,20)
  io.wrIdx := io.instruction(11,7)
  switch(insOpcode) {
    is(Opcode.Alu.U) {
      switch(funct3) {
        is(AluFunct3.ADD_SUB.U) {
          when(funct7 === "h20".U) {
            io.insType := insType.SUB.U
          } .otherwise {
            io.insType := insType.ADD.U
          }
        }
        is(AluFunct3.SLL.U) {
          io.insType := insType.SLL.U
        }
        is(AluFunct3.SLT.U) {
          io.insType := insType.SLT.U
        }
        is(AluFunct3.SLTU.U) {
          io.insType := insType.SLTU.U
        }
        is(AluFunct3.XOR.U) {
          io.insType := insType.XOR.U
        }
        is(AluFunct3.SRL_SRA.U) {
          when(funct7 === "h20".U) {
            io.insType := insType.SRA.U
          } .otherwise {
            io.insType := insType.SRL.U
          }
        }
        is(AluFunct3.OR.U) {
          io.insType := insType.OR.U
        }
        is(AluFunct3.AND.U) {
          io.insType := insType.AND.U
        }
      }
    }
    is(Opcode.AluImm.U) {
      io.imm := io.instruction(31,20)
      switch(funct3) {
        is(AluImmFunct3.ADDI.U) {
          io.insType := insType.ADDI.U
        }
        is(AluImmFunct3.SLLI.U) {
          io.insType := insType.SLLI.U
        }
        is(AluImmFunct3.SLTI.U) {
          io.insType := insType.SLTI.U
        }
        is(AluImmFunct3.SLTIU.U) {
          io.insType := insType.SLTIU.U
        }
        is(AluImmFunct3.XORI.U) {
          io.insType := insType.XORI.U
        }
        is(AluImmFunct3.SRLI_SRAI.U) {
          when(funct7 === "h20".U) {
            io.insType := insType.SRAI.U
          } .otherwise {
            io.insType := insType.SRLI.U
          }
        }
        is(AluImmFunct3.ORI.U) {
          io.insType := insType.ORI.U
        }
        is(AluImmFunct3.ANDI.U) {
          io.insType := insType.ADDI.U
        }
      }

    }
    is(Opcode.Branch.U) {
      val imm10_5 = io.instruction(30,25)
      val imm4_1 = io.instruction(11,8)
      val imm11 = io.instruction(7)
      val imm12 = io.instruction(31)
      io.imm := Cat(imm12, imm11, imm10_5, imm4_1, 0.U)
      switch(funct3) {
        is(BranchFunct.BEQ.U) {
          io.insType := insType.BEQ.U
        }
        is(BranchFunct.BNE.U) {
          io.insType := insType.BNE.U
        }
        is(BranchFunct.BLT.U) {
          io.insType := insType.BLT.U
        }
        is(BranchFunct.BGE.U) {
          io.insType := insType.BGE.U
        }
        is(BranchFunct.BLTU.U) {
          io.insType := insType.BLTU.U
        }
        is(BranchFunct.BGEU.U) {
          io.insType := insType.BGEU.U
        }
      }

    }
    is(Opcode.Load.U) {
      io.imm := io.instruction(31,20)
      switch(funct3) {
        is(LoadFunct.LB.U) {
          io.insType := insType.LB.U
        }
        is(LoadFunct.LH.U) {
          io.insType := insType.LH.U
        }
        is(LoadFunct.LW.U) {
          io.insType := insType.LW.U
        }
        is(LoadFunct.LBU.U) {
          io.insType := insType.LBU.U
        }
        is(LoadFunct.LHU.U) {
          io.insType := insType.LHU.U
        }
      }
    }
    is(Opcode.Store.U) {
      val imm4 = io.instruction(11,7)
      val imm11_5 = io.instruction(31,25)
      io.imm := Cat(imm11_5, imm4)
      switch(funct3) {
        is(StoreFunct.SB.U) {
          io.insType := insType.SB.U
        }
        is(StoreFunct.SH.U) {
          io.insType := insType.SH.U
        }
        is(StoreFunct.SW.U) {
          io.insType := insType.SW.U
        }
      }

    }
    is(Opcode.Lui.U) {
      io.imm := Cat(io.instruction(31,12), "h000".U)
      io.insType := insType.LUI.U
    }
    is(Opcode.AuiPc.U) {
      io.imm := Cat(io.instruction(31,12), "h000".U)
      io.insType := insType.AUIPC.U
    }
    is(Opcode.Jal.U) {
      val imm20 = io.instruction(31)
      val imm19_12 = io.instruction(19,12)
      val imm11 = io.instruction(20)
      val imm10_1 = io.instruction(30,21)
      io.imm := Cat(imm20, imm19_12, imm11, imm10_1, 0.U)
      io.insType := insType.JAL.U
    }
    is(Opcode.JalR.U) {
      io.imm := io.instruction(31,20)
      io.insType := insType.JALR.U
    }
    is(Opcode.Fence.U) {
      //TODO
      io.insType := insType.FENCE.U
    }
    is(Opcode.ECall.U) {
      when(funct7 === 0.U) {
        io.insType := insType.ECALL.U
      } .otherwise {
        io.insType := insType.EBREAK.U
      }
    }
  }

}

package riscv

import chisel3._
import chisel3.util._

class Decode extends Module {
  val io = IO(new Bundle() {
    val FeDec = Input(new FeDec())
    val DecEx = Output(new DecEx())
    val aluOp = Output(UInt(6.W))
    val types = Output(UInt(5.W))
  })
  val insOpcode = io.FeDec.instruction(6, 0)

  val funct3 = io.FeDec.instruction(14, 12)
  val funct7 = io.FeDec.instruction(31, 25)
  val rs1Idx = io.FeDec.instruction(19, 15)
  val rs2Idx = io.FeDec.instruction(24, 20)
  val wrIdx = io.FeDec.instruction(11, 7)
  val imm = DontCare
  io.types := DontCare
  io.aluOp := DontCare

  switch(insOpcode) {
    is(Opcode.Alu.U) {
      io.types := Types.R.id.U
      switch(funct3) {
        is(AluFunct3.ADD_SUB.U) {
          when(funct7 === "h20".U) {
            io.aluOp := AluType.SUB.id.U
          }.otherwise {
            io.aluOp := AluType.ADD.id.U
          }
        }
        is(AluFunct3.SLL.U) {
          io.aluOp := AluType.SLL.id.U
        }
        is(AluFunct3.SLT.U) {
          io.aluOp := AluType.SLT.id.U
        }
        is(AluFunct3.SLTU.U) {
          io.aluOp := AluType.SLTU.id.U
        }
        is(AluFunct3.XOR.U) {
          io.aluOp := AluType.XOR.id.U
        }
        is(AluFunct3.SRL_SRA.U) {
          when(funct7 === "h20".U) {
            io.aluOp := AluType.SRA.id.U
          }.otherwise {
            io.aluOp := AluType.SRL.id.U
          }
        }
        is(AluFunct3.OR.U) {
          io.aluOp := AluType.OR.id.U
        }
        is(AluFunct3.AND.U) {
          io.aluOp := AluType.AND.id.U
        }
      }
    }
    is(Opcode.AluImm.U) {
      io.types := Types.I.id.U
      //sign extend imm
      val slice = io.FeDec.instruction(31, 20)
      val signBit = slice(11)
      imm := Cat(Fill(20, signBit), slice)
      switch(funct3) {
        is(AluImmFunct3.ADDI.U) {
          io.aluOp := AluType.ADD.id.U
        }
        is(AluImmFunct3.SLLI.U) {
          io.aluOp := AluType.SLL.id.U
        }
        is(AluImmFunct3.SLTI.U) {
          io.aluOp := AluType.SLT.id.U
        }
        is(AluImmFunct3.SLTIU.U) {
          io.aluOp := AluType.SLTU.id.U
        }
        is(AluImmFunct3.XORI.U) {
          io.aluOp := AluType.XOR.id.U
        }
        is(AluImmFunct3.SRLI_SRAI.U) {
          imm := io.FeDec.instruction(24, 20)
          when(funct7 === "h20".U) {
            io.aluOp := AluType.SRA.id.U
          }.otherwise {
            io.aluOp := AluType.SRL.id.U
          }
        }
        is(AluImmFunct3.ORI.U) {
          io.aluOp := AluType.OR.id.U
        }
        is(AluImmFunct3.ANDI.U) {
          io.aluOp := AluType.AND.id.U
        }
      }

    }
    is(Opcode.Branch.U) {
      io.types := Types.B.id.U
      val imm10_5 = io.FeDec.instruction(30, 25)
      val imm4_1 = io.FeDec.instruction(11, 8)
      val imm11 = io.FeDec.instruction(7)
      val imm12 = io.FeDec.instruction(31)
      //the fill is for sign extention
      imm := Cat(Fill(20,imm12),imm12, imm11, imm10_5, imm4_1, 0.U)
      io.aluOp := funct3
    }
    is(Opcode.Load.U) {
      io.types := Types.LOAD.id.U
      val slice = io.FeDec.instruction(31, 20)
      val signBit = slice(11)
      imm := Cat(Fill(20, signBit), slice)
      io.aluOp := funct3
    }
    is(Opcode.Store.U) {
      io.types := Types.S.id.U
      val imm4 = io.FeDec.instruction(11, 7)
      val imm11_5 = io.FeDec.instruction(31, 25)
      val signBit = imm11_5(6)
      imm := Cat(Fill(20,signBit), imm11_5, imm4)
      io.aluOp := funct3
    }
    is(Opcode.Lui.U) {
      io.types := Types.U.id.U
      imm := Cat(io.FeDec.instruction(31, 12), "b0".U(12.W))
      io.aluOp := insType.LUI.U
    }
    is(Opcode.AuiPc.U) {
      io.types := Types.U.id.U
      imm := Cat(io.FeDec.instruction(31, 12), "b0".U(12.W))
      io.aluOp := insType.AUIPC.U
    }
    is(Opcode.Jal.U) {
      io.types := Types.J.id.U
      val imm20 = io.FeDec.instruction(31)
      val imm19_12 = io.FeDec.instruction(19, 12)
      val imm11 = io.FeDec.instruction(20)
      val imm10_1 = io.FeDec.instruction(30, 21)
      imm := Cat(Fill(12, imm20), imm20, imm19_12, imm11, imm10_1, 0.U)
      io.aluOp := insType.JAL.U
    }
    is(Opcode.JalR.U) {
      io.types := Types.J.id.U
      //sign extend imm
      val slice = io.FeDec.instruction(31, 20)
      val signBit = slice(11)
      imm := Cat(Fill(20, signBit), slice)
      io.aluOp := insType.JALR.U
    }
    is(Opcode.Fence.U) {
      //TODO
      io.aluOp := insType.FENCE.U
    }
    is(Opcode.ECall.U) {
      io.types := Types.ECALL.id.U
      val isEBreak = io.FeDec.instruction(20)
      when(isEBreak === 1.U) {
        io.aluOp := insType.EBREAK.U
      }.otherwise {
        io.aluOp := insType.ECALL.U
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
  




  //REGISTER FILE
  val reg1 = DontCare
  val reg2 = DontCare
  val regWrite = false.B
  val dataIn = DontCare

  val registers = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  registers(0) := 0.U

  reg1 := registers(rs1Idx)
  reg2 := registers(rs2Idx)


  when(regWrite) {
    registers(wrIdx) := dataIn
  }

  io.DecEx.imm := imm
  io.DecEx.regData1 := reg1
  io.DecEx.regData2 := reg2



  

}

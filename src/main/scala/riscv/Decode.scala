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
  val types = Wire(UInt(6.W))
  val aluOp = Wire(UInt(5.W))
  types := DontCare
  aluOp := DontCare

  switch(insOpcode) {
    is(Opcode.Alu.U) {
      types := Types.R.id.U
      switch(funct3) {
        is(AluFunct3.ADD_SUB.U) {
          when(funct7 === "h20".U) {
            aluOp := AluType.SUB.id.U
          }.otherwise {
            aluOp := AluType.ADD.id.U
          }
        }
        is(AluFunct3.SLL.U) {
          aluOp := AluType.SLL.id.U
        }
        is(AluFunct3.SLT.U) {
          aluOp := AluType.SLT.id.U
        }
        is(AluFunct3.SLTU.U) {
          aluOp := AluType.SLTU.id.U
        }
        is(AluFunct3.XOR.U) {
          aluOp := AluType.XOR.id.U
        }
        is(AluFunct3.SRL_SRA.U) {
          when(funct7 === "h20".U) {
            aluOp := AluType.SRA.id.U
          }.otherwise {
            aluOp := AluType.SRL.id.U
          }
        }
        is(AluFunct3.OR.U) {
          aluOp := AluType.OR.id.U
        }
        is(AluFunct3.AND.U) {
          aluOp := AluType.AND.id.U
        }
      }
    }
    is(Opcode.AluImm.U) {
      types := Types.I.id.U
      //sign extend imm
      val slice = io.FeDec.instruction(31, 20)
      val signBit = slice(11)
      imm := Cat(Fill(20, signBit), slice)
      switch(funct3) {
        is(AluImmFunct3.ADDI.U) {
          aluOp := AluType.ADD.id.U
        }
        is(AluImmFunct3.SLLI.U) {
          aluOp := AluType.SLL.id.U
        }
        is(AluImmFunct3.SLTI.U) {
          aluOp := AluType.SLT.id.U
        }
        is(AluImmFunct3.SLTIU.U) {
          aluOp := AluType.SLTU.id.U
        }
        is(AluImmFunct3.XORI.U) {
          aluOp := AluType.XOR.id.U
        }
        is(AluImmFunct3.SRLI_SRAI.U) {
          imm := io.FeDec.instruction(24, 20)
          when(funct7 === "h20".U) {
            aluOp := AluType.SRA.id.U
          }.otherwise {
            aluOp := AluType.SRL.id.U
          }
        }
        is(AluImmFunct3.ORI.U) {
          aluOp := AluType.OR.id.U
        }
        is(AluImmFunct3.ANDI.U) {
          aluOp := AluType.AND.id.U
        }
      }

    }
    is(Opcode.Branch.U) {
      types := Types.B.id.U
      val imm10_5 = io.FeDec.instruction(30, 25)
      val imm4_1 = io.FeDec.instruction(11, 8)
      val imm11 = io.FeDec.instruction(7)
      val imm12 = io.FeDec.instruction(31)
      //the fill is for sign extention
      imm := Cat(Fill(20,imm12),imm12, imm11, imm10_5, imm4_1, 0.U)
      aluOp := funct3
    }
    is(Opcode.Load.U) {
      types := Types.LOAD.id.U
      val slice = io.FeDec.instruction(31, 20)
      val signBit = slice(11)
      imm := Cat(Fill(20, signBit), slice)
      aluOp := funct3
    }
    is(Opcode.Store.U) {
      types := Types.S.id.U
      val imm4 = io.FeDec.instruction(11, 7)
      val imm11_5 = io.FeDec.instruction(31, 25)
      val signBit = imm11_5(6)
      imm := Cat(Fill(20,signBit), imm11_5, imm4)
      aluOp := funct3
    }
    is(Opcode.Lui.U) {
      types := Types.U.id.U
      imm := Cat(io.FeDec.instruction(31, 12), "b0".U(12.W))
      aluOp := insType.LUI.U
    }
    is(Opcode.AuiPc.U) {
      types := Types.U.id.U
      imm := Cat(io.FeDec.instruction(31, 12), "b0".U(12.W))
      aluOp := insType.AUIPC.U
    }
    is(Opcode.Jal.U) {
      types := Types.J.id.U
      val imm20 = io.FeDec.instruction(31)
      val imm19_12 = io.FeDec.instruction(19, 12)
      val imm11 = io.FeDec.instruction(20)
      val imm10_1 = io.FeDec.instruction(30, 21)
      imm := Cat(Fill(12, imm20), imm20, imm19_12, imm11, imm10_1, 0.U)
      aluOp := insType.JAL.U
    }
    is(Opcode.JalR.U) {
      types := Types.J.id.U
      //sign extend imm
      val slice = io.FeDec.instruction(31, 20)
      val signBit = slice(11)
      imm := Cat(Fill(20, signBit), slice)
      aluOp := insType.JALR.U
    }
    is(Opcode.Fence.U) {
      //TODO
      aluOp := insType.FENCE.U
    }
    is(Opcode.ECall.U) {
      types := Types.ECALL.id.U
      val isEBreak = io.FeDec.instruction(20)
      when(isEBreak === 1.U) {
        aluOp := insType.EBREAK.U
      }.otherwise {
        aluOp := insType.ECALL.U
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




  val control = Module(new Control)
  control.io.aluOp := aluOp
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


  

}

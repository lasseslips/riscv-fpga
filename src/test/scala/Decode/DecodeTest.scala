/*
package Decode

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.{AluFunct3, AluType, Decode, LoadStoreFunct, Types}


class DecodeTest extends AnyFlatSpec with ChiselScalatestTester{
  "DecodeTest" should "Pass" in {
    test(new Decode) { dut =>
      //add
      dut.io.instruction.poke("b00000000000100010000000010110011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.rs2Idx.expect(1.U)
      dut.io.insType.expect(AluType.ADD.id.U)
      //sub
      dut.io.instruction.poke("b01000000001100010000000010110011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.rs2Idx.expect(3.U)
      dut.io.insType.expect(AluType.SUB.id.U)

      //sll
      dut.io.instruction.poke("b00000000001100010001000010110011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.rs2Idx.expect(3.U)
      dut.io.insType.expect(AluType.SLL.id.U)
      //slt 
      dut.io.instruction.poke("b00000000001100010010000010110011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.rs2Idx.expect(3.U)
      dut.io.insType.expect(AluType.SLT.id.U)
      //sltu
      dut.io.instruction.poke("b00000000001100010011000010110011".U)

      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.rs2Idx.expect(3.U)
      dut.io.insType.expect(AluType.SLTU.id.U)
      //xor
      dut.io.instruction.poke("b00000000001100010100000010110011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.rs2Idx.expect(3.U)
      dut.io.insType.expect(AluType.XOR.id.U)
      //srl
      dut.io.instruction.poke("b00000000001100010101000010110011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.rs2Idx.expect(3.U)
      dut.io.insType.expect(AluType.SRL.id.U)
      //sra
      dut.io.instruction.poke("b01000000001100010101000010110011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.rs2Idx.expect(3.U)
      dut.io.insType.expect(AluType.SRA.id.U)
      //or
      dut.io.instruction.poke("b00000000001100010110000010110011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.rs2Idx.expect(3.U)
      dut.io.insType.expect(AluType.OR.id.U)
      //and
      dut.io.instruction.poke("b00000000001100010111000010110011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.rs2Idx.expect(3.U)
      dut.io.insType.expect(AluType.AND.id.U)
      //addi
      dut.io.instruction.poke("b00000000101000010000000010010011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(10.U)

      dut.io.insType.expect(AluType.ADD.id.U)

      //SLTI
      dut.io.instruction.poke("b00000000001100010010000010010011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(3.U)
      dut.io.insType.expect(AluType.SLT.id.U)
      //SLTIU
      dut.io.instruction.poke("b00000000001100010011000010010011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(3.U)
      dut.io.insType.expect(AluType.SLTU.id.U)
      //XORI    
      dut.io.instruction.poke("b00000000001100010100000010010011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(3.U)
      dut.io.insType.expect(AluType.XOR.id.U)
      //ORI   
      dut.io.instruction.poke("b00000000001100010110000010010011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(3.U)
      dut.io.insType.expect(AluType.OR.id.U)
      //ANDI
      dut.io.instruction.poke("b00000000001100010111000010010011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(3.U)
      dut.io.insType.expect(AluType.AND.id.U)
      //SLLI
      dut.io.instruction.poke("b00000000001100010001000010010011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(3.U)
      dut.io.insType.expect(AluType.SLL.id.U)
      //SRLI
      dut.io.instruction.poke("b00000000001100010101000010010011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(3.U)
      dut.io.insType.expect(AluType.SRL.id.U)
      //SRAI
      dut.io.instruction.poke("b01000000001100010101000010010011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(3.U)
      dut.io.insType.expect(AluType.SRA.id.U)
      //BEQ
      dut.io.instruction.poke("b00000000001000001000000101100011".U)
      dut.io.rs1Idx.expect(1.U)
      dut.io.rs2Idx.expect(2.U)
      dut.io.imm.expect(2.U)
      dut.io.insType.expect(0.U)
      //BNE
      dut.io.instruction.poke("b00000000001000001001000101100011".U)
      dut.io.rs1Idx.expect(1.U)
      dut.io.rs2Idx.expect(2.U)
      dut.io.imm.expect(2.U)
      dut.io.insType.expect(1.U)
      //BLT
      dut.io.instruction.poke("b00000000001000001100000101100011".U)
      dut.io.rs1Idx.expect(1.U)
      dut.io.rs2Idx.expect(2.U)
      dut.io.imm.expect(2.U)
      dut.io.insType.expect(4.U)
      //BGE
      dut.io.instruction.poke("b00000000111101110101011001100011".U)
      dut.io.rs1Idx.expect(14.U)
      dut.io.rs2Idx.expect(15.U)
      dut.io.imm.expect(12.U)
      dut.io.insType.expect(5.U)
      //BLTU
      dut.io.instruction.poke("b00000000001000001110000101100011".U)
      dut.io.rs1Idx.expect(1.U)
      dut.io.rs2Idx.expect(2.U)
      dut.io.imm.expect(2.U)
      dut.io.insType.expect(6.U)
      //BGEU
      dut.io.instruction.poke("b00000000001000001111000101100011".U)
      dut.io.rs1Idx.expect(1.U)
      dut.io.rs2Idx.expect(2.U)
      dut.io.imm.expect(2.U)
      dut.io.insType.expect(7.U)
      //LB
      dut.io.instruction.poke("b00000000001100010000000010000011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(3.U)
      dut.io.types.expect(Types.LOAD.id.U)
      dut.io.insType.expect(LoadStoreFunct.LB_SB)
      //LH
      dut.io.instruction.poke("b00000000001100010001000010000011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(3.U)
      dut.io.types.expect(Types.LOAD.id.U)
      dut.io.insType.expect(LoadStoreFunct.LH_SH)
      //LW
      dut.io.instruction.poke("b00000000001100010010000010000011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(3.U)
      dut.io.types.expect(Types.LOAD.id.U)
      dut.io.insType.expect(LoadStoreFunct.LW_SW)
      //LBU
      dut.io.instruction.poke("b00000000001100010100000010000011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(3.U)
      dut.io.types.expect(Types.LOAD.id.U)
      dut.io.insType.expect(LoadStoreFunct.LBU)
      //LHU
      dut.io.instruction.poke("b00000000001100010101000010000011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(3.U)
      dut.io.types.expect(Types.LOAD.id.U)
      dut.io.insType.expect(LoadStoreFunct.LHU)
      //SB
      dut.io.instruction.poke("b00000000000100010000000110100011".U)
      dut.io.rs2Idx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(3.U)
      dut.io.types.expect(Types.S.id.U)
      dut.io.insType.expect(LoadStoreFunct.LB_SB)
      //SH
      dut.io.instruction.poke("b00000000000100010001000110100011".U)
      dut.io.rs2Idx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(3.U)
      dut.io.types.expect(Types.S.id.U)
      dut.io.insType.expect(LoadStoreFunct.LH_SH)
      //SW
      dut.io.instruction.poke("b00000000000100010010000110100011".U)
      dut.io.rs2Idx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(3.U)
      dut.io.types.expect(Types.S.id.U)
      dut.io.insType.expect(LoadStoreFunct.LW_SW)
      //LUI
      dut.io.instruction.poke("b00000000000000000010000010110111".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.imm.expect((2 << 11).U)
      dut.io.insType.expect(33.U)
      //AUIPC
      dut.io.instruction.poke("b00000000000000000010000010010111".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.imm.expect((2 << 11).U)
      dut.io.insType.expect(34.U)
      //JAL
      dut.io.instruction.poke("b00000000001000000000000011101111".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.imm.expect(2.U)
      dut.io.insType.expect(35.U)
      //JALR
      dut.io.instruction.poke("b00000000001100010000000011100111".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.imm.expect(3.U)
      dut.io.insType.expect(36.U)
      //FENCE
      //TODO
      //ECALL
      dut.io.instruction.poke("b00000000000000000000000001110011".U)
      dut.io.insType.expect(38.U)

      //EBREAK
      dut.io.instruction.poke("b00000000000100000000000001110011".U)
      dut.io.insType.expect(39.U)
      }
    }
}


 */
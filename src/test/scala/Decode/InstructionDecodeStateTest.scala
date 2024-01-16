package Decode

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.lib.{AluFunct3, AluType, LoadStoreFunct, Opcode, RegWriteSrc, Types, insType}
import riscv.stages.InstructionDecode


class InstructionDecodeStateTest extends AnyFlatSpec with ChiselScalatestTester {
  "DecodeStageTest" should "Pass" in {
    test(new InstructionDecode) { dut =>
      //add
      dut.io.FeDec.instruction.poke("b00000000000000000000001010110011".U)
      dut.io.FeDec.pc.poke(12.U)
      dut.clock.step()
      dut.io.DecEx.pc.expect(12.U)
      dut.io.DecEx.regWrIdx.expect(5.U)
      dut.io.DecEx.regData1.expect(0.U)
      dut.io.DecEx.regData2.expect(0.U)
      dut.io.DecEx.regWrite.expect(true.B)
      dut.io.DecEx.memWrite.expect(false.B)
      dut.io.DecEx.regWriteSrc.expect(RegWriteSrc.ALU.id.U)
      dut.io.DecEx.aluSrc.expect("b00".U)
      dut.io.DecEx.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.DecEx.branchEnable.expect(false.B)
      dut.io.DecEx.jumpEnable.expect(false)

      //addi
      dut.io.FeDec.instruction.poke("b00000110010000000000001010010011".U)
      dut.io.FeDec.pc.poke(12.U)
      dut.clock.step()
      dut.io.DecEx.pc.expect(12.U)
      dut.io.DecEx.regWrIdx.expect(5.U)
      dut.io.DecEx.regData1.expect(0.U)
      dut.io.DecEx.imm.expect(100.U)
      dut.io.DecEx.regWrite.expect(true.B)
      dut.io.DecEx.memWrite.expect(false.B)
      dut.io.DecEx.regWriteSrc.expect(RegWriteSrc.ALU.id.U)
      dut.io.DecEx.aluSrc.expect("b01".U)
      dut.io.DecEx.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.DecEx.branchEnable.expect(false.B)
      dut.io.DecEx.jumpEnable.expect(false)

      //beq
      dut.io.FeDec.instruction.poke("b00000110000000000000001001100011".U)
      dut.io.FeDec.pc.poke(12.U)
      dut.clock.step()
      dut.io.DecEx.pc.expect(12.U)
      dut.io.DecEx.regData1.expect(0.U)
      dut.io.DecEx.regData2.expect(0.U)
      dut.io.DecEx.regWrite.expect(false.B)
      dut.io.DecEx.memWrite.expect(false.B)
      dut.io.DecEx.aluSrc.expect("b11".U)
      dut.io.DecEx.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.DecEx.branchEnable.expect(true.B)
      dut.io.DecEx.jumpEnable.expect(false)

      //lw
      dut.io.FeDec.instruction.poke("b00000110010000000010001010000011".U)
      dut.io.FeDec.pc.poke(12.U)
      dut.clock.step()
      dut.io.DecEx.pc.expect(12.U)
      dut.io.DecEx.regData1.expect(0.U)
      dut.io.DecEx.regWrIdx.expect(5.U)
      dut.io.DecEx.imm.expect(100.U)
      dut.io.DecEx.regWrite.expect(true.B)
      dut.io.DecEx.memWrite.expect(false.B)
      dut.io.DecEx.aluSrc.expect("b01".U)
      dut.io.DecEx.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.DecEx.regWriteSrc.expect(RegWriteSrc.MEMORY.id.U)
      dut.io.DecEx.branchEnable.expect(false.B)
      dut.io.DecEx.jumpEnable.expect(false)

      //sw
      dut.io.FeDec.instruction.poke("b00000110000000000010001000100011".U)
      dut.io.FeDec.pc.poke(12.U)
      dut.clock.step()
      dut.io.DecEx.pc.expect(12.U)
      dut.io.DecEx.regData1.expect(0.U)
      dut.io.DecEx.regData2.expect(0.U)
      dut.io.DecEx.imm.expect(100.U)
      dut.io.DecEx.regWrite.expect(false.B)
      dut.io.DecEx.memWrite.expect(true.B)
      dut.io.DecEx.aluSrc.expect("b01".U)
      dut.io.DecEx.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.DecEx.memWrite.expect(true.B)
      dut.io.DecEx.memIns.expect(LoadStoreFunct.LW_SW.U)
      dut.io.DecEx.branchEnable.expect(false.B)
      dut.io.DecEx.jumpEnable.expect(false)


      //lui
      dut.io.FeDec.instruction.poke("b00000000000000000010001010110111".U)
      dut.io.FeDec.pc.poke(12.U)
      dut.clock.step()
      dut.io.DecEx.pc.expect(12.U)
      dut.io.DecEx.regData1.expect(0.U)
      dut.io.DecEx.regWrIdx.expect(5.U)
      dut.io.DecEx.imm.expect((2 << 12).U)
      dut.io.DecEx.regWrite.expect(true.B)
      dut.io.DecEx.memWrite.expect(false.B)
      dut.io.DecEx.aluSrc.expect("b01".U)
      dut.io.DecEx.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.DecEx.regWriteSrc.expect(RegWriteSrc.ALU.id.U)
      dut.io.DecEx.memWrite.expect(false.B)
      dut.io.DecEx.branchEnable.expect(false.B)
      dut.io.DecEx.jumpEnable.expect(false)

      //AUIPC
      dut.io.FeDec.instruction.poke("b00000000000000000010001010010111".U)
      dut.io.FeDec.pc.poke(12.U)
      dut.clock.step()
      dut.io.DecEx.pc.expect(12.U)
      dut.io.DecEx.regWrIdx.expect(5.U)
      dut.io.DecEx.imm.expect((2 << 12).U)
      dut.io.DecEx.regWrite.expect(true.B)
      dut.io.DecEx.memWrite.expect(false.B)
      dut.io.DecEx.aluSrc.expect("b11".U)
      dut.io.DecEx.regWriteSrc.expect(RegWriteSrc.ALU.id.U)
      dut.io.DecEx.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.DecEx.memWrite.expect(false.B)
      dut.io.DecEx.branchEnable.expect(false.B)
      dut.io.DecEx.jumpEnable.expect(false)

      //jal
      dut.io.FeDec.instruction.poke("b00000110010000000000001011101111".U)
      dut.io.FeDec.pc.poke(12.U)
      dut.clock.step()
      dut.io.DecEx.pc.expect(12.U)
      dut.io.DecEx.regWrIdx.expect(5.U)
      dut.io.DecEx.imm.expect(100.U)
      dut.io.DecEx.regWrite.expect(true.B)
      dut.io.DecEx.memWrite.expect(false.B)
      dut.io.DecEx.aluSrc.expect("b11".U)
      dut.io.DecEx.regWriteSrc.expect(RegWriteSrc.PC.id.U)
      dut.io.DecEx.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.DecEx.memWrite.expect(false.B)
      dut.io.DecEx.branchEnable.expect(false.B)
      dut.io.DecEx.jumpEnable.expect(true)

      //jalr
      dut.io.FeDec.instruction.poke("b00000110010000000000001011100111".U)
      dut.io.FeDec.pc.poke(12.U)
      dut.clock.step()
      dut.io.DecEx.pc.expect(12.U)
      dut.io.DecEx.regWrIdx.expect(5.U)
      dut.io.DecEx.imm.expect(100.U)
      dut.io.DecEx.regWrite.expect(true.B)
      dut.io.DecEx.memWrite.expect(false.B)
      dut.io.DecEx.aluSrc.expect("b01".U)
      dut.io.DecEx.regWriteSrc.expect(RegWriteSrc.PC.id.U)
      dut.io.DecEx.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.DecEx.memWrite.expect(false.B)
      dut.io.DecEx.branchEnable.expect(false.B)
      dut.io.DecEx.jumpEnable.expect(true)
    }
  }
}

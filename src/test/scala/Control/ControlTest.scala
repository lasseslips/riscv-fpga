package Control

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.{AluType, BranchFunct, Control, LoadStoreFunct, RegWriteSrc, Types, insType}

class ControlTest extends AnyFlatSpec with ChiselScalatestTester{
  "Control" should "Pass" in {
    test(new Control) { dut =>
      dut.io.insType.poke(AluType.ADD.id.U)
      dut.io.types.poke(Types.R.id.U)
      dut.io.regWrite.expect(true.B)
      dut.io.memWrite.expect(false.B)
      dut.io.regWriteSrc.expect(RegWriteSrc.ALU.id.U)
      dut.io.aluSrc.expect("b00".U)
      dut.io.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.branchEnable.expect(false.B)
      dut.io.jumpEnable.expect(false.B)

      dut.io.insType.poke(AluType.ADD.id.U)
      dut.io.types.poke(Types.I.id.U)
      dut.io.regWrite.expect(true.B)
      dut.io.memWrite.expect(false.B)
      dut.io.regWriteSrc.expect(RegWriteSrc.ALU.id.U)
      dut.io.aluSrc.expect("b01".U)
      dut.io.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.branchEnable.expect(false.B)
      dut.io.jumpEnable.expect(false.B)

      dut.io.insType.poke(BranchFunct.BEQ.U)
      dut.io.types.poke(Types.B.id.U)
      dut.io.regWrite.expect(false.B)
      dut.io.memWrite.expect(false.B)
      dut.io.aluSrc.expect("b11".U)
      dut.io.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.branchEnable.expect(true.B)
      dut.io.jumpEnable.expect(false.B)

      dut.io.insType.poke(LoadStoreFunct.LB_SB.U)
      dut.io.types.poke(Types.LOAD.id.U)
      dut.io.regWrite.expect(true.B)
      dut.io.memWrite.expect(false.B)
      dut.io.regWriteSrc.expect(RegWriteSrc.MEMORY.id.U)
      dut.io.aluSrc.expect("b01".U)
      dut.io.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.branchEnable.expect(false.B)
      dut.io.jumpEnable.expect(false.B)

      dut.io.insType.poke(LoadStoreFunct.LB_SB.U)
      dut.io.types.poke(Types.S.id.U)
      dut.io.regWrite.expect(false.B)
      dut.io.memWrite.expect(true.B)
      dut.io.aluSrc.expect("b01".U)
      dut.io.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.branchEnable.expect(false.B)
      dut.io.jumpEnable.expect(false.B)

      dut.io.insType.poke(insType.LUI.U)
      dut.io.types.poke(Types.U.id.U)
      dut.io.regWrite.expect(true.B)
      dut.io.aluSrc.expect("b01".U)
      dut.io.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.branchEnable.expect(false.B)
      dut.io.jumpEnable.expect(false.B)

      dut.io.insType.poke(insType.AUIPC.U)
      dut.io.types.poke(Types.U.id.U)
      dut.io.regWrite.expect(true.B)
      dut.io.aluSrc.expect("b11".U)
      dut.io.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.branchEnable.expect(false.B)
      dut.io.jumpEnable.expect(false.B)

      dut.io.insType.poke(insType.JAL.U)
      dut.io.types.poke(Types.J.id.U)
      dut.io.regWrite.expect(true.B)
      dut.io.memWrite.expect(false.B)
      dut.io.aluSrc.expect("b11".U)
      dut.io.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.branchEnable.expect(false.B)
      dut.io.jumpEnable.expect(true.B)

      dut.io.insType.poke(insType.JALR.U)
      dut.io.types.poke(Types.J.id.U)
      dut.io.regWrite.expect(true.B)
      dut.io.memWrite.expect(false.B)
      dut.io.aluSrc.expect("b01".U)
      dut.io.aluOpcode.expect(AluType.ADD.id.U)
      dut.io.branchEnable.expect(false.B)
      dut.io.jumpEnable.expect(true.B)

    }
  }
}

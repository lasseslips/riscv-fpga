/*
package Branch

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.{Branch, BranchFunct}

class BranchTest extends AnyFlatSpec with ChiselScalatestTester {
  "Control" should "Pass" in {
    test(new Branch) { dut =>
      dut.io.branchEnable.poke(true.B)
      dut.io.rs1.poke(55.U)
      dut.io.rs2.poke(55.U)
      dut.io.branchType.poke(BranchFunct.BEQ.U)
      dut.io.branching.expect(true.B)

      dut.io.branchEnable.poke(true.B)
      dut.io.rs1.poke(555.U)
      dut.io.rs2.poke(55.U)
      dut.io.branchType.poke(BranchFunct.BNE.U)
      dut.io.branching.expect(true.B)

      dut.io.branchEnable.poke(true.B)
      dut.io.rs1.poke(55.U)
      dut.io.rs2.poke(555.U)
      dut.io.branchType.poke(BranchFunct.BLT.U)
      dut.io.branching.expect(true.B)

      dut.io.branchEnable.poke(true.B)
      dut.io.rs1.poke(555.U)
      dut.io.rs2.poke(55.U)
      dut.io.branchType.poke(BranchFunct.BGE.U)
      dut.io.branching.expect(true.B)

      dut.io.branchEnable.poke(true.B)
      dut.io.rs1.poke("hFFFFFFFF".U)
      dut.io.rs2.poke(55.U)
      dut.io.branchType.poke(BranchFunct.BGE.U)
      dut.io.branching.expect(false.B)

      dut.io.branchEnable.poke(true.B)
      dut.io.rs1.poke("hFFFFFFFF".U)
      dut.io.rs2.poke(55.U)
      dut.io.branchType.poke(BranchFunct.BGEU.U)
      dut.io.branching.expect(true.B)

      dut.io.branchEnable.poke(true.B)
      dut.io.rs1.poke(5)
      dut.io.rs2.poke("hFFFFFFFF".U)
      dut.io.branchType.poke(BranchFunct.BLTU.U)
      dut.io.branching.expect(true.B)

      dut.io.branchEnable.poke(false.B)
      dut.io.rs1.poke(5)
      dut.io.rs2.poke("hFFFFFFFF".U)
      dut.io.branchType.poke(BranchFunct.BLTU.U)
      dut.io.branching.expect(false.B)


    }
  }
}


 */
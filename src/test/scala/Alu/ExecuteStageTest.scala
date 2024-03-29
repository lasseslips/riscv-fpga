package Alu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.lib.BranchFunct
import riscv.stages.Execute

class ExecuteStageTest extends AnyFlatSpec with ChiselScalatestTester{
  "ALUSTAGE" should "Pass" in {
    test(new Execute) { dut =>

      dut.io.DecEx.regData1.poke(14.U)
      dut.io.DecEx.regData2.poke(14.U)
      dut.io.DecEx.branchType.poke(BranchFunct.BEQ.U)
      dut.io.DecEx.branchEnable.poke(true.B)
      dut.clock.step()
      dut.io.ExFe.jump.expect(true.B)

      dut.io.DecEx.regData1.poke(14.U)
      dut.io.DecEx.regData2.poke(14.U)
      dut.io.DecEx.branchType.poke(BranchFunct.BEQ.U)
      dut.io.DecEx.branchEnable.poke(false.B)
      dut.io.DecEx.jumpEnable.poke(false.B)
      dut.clock.step()
      dut.io.ExFe.jump.expect(false.B)
      dut.io.DecEx.jumpEnable.poke(true.B)
      dut.clock.step()
      dut.io.ExFe.jump.expect(true.B)
    }
  }
}

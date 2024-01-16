package Alu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.stages.Execute

class SubTest extends AnyFlatSpec with ChiselScalatestTester{
  "SUB" should "Pass" in {
    test(new Execute) { dut =>
      dut.io.DecEx.aluOpcode.poke(1.U)
      dut.io.DecEx.aluSrc.poke("b00".U)

      dut.io.DecEx.regData1.poke(1.U)
      dut.io.DecEx.regData2.poke(2.U)
      dut.clock.step()
      dut.io.ExMem.addr.expect("hFFFFFFFF".U)

      dut.io.DecEx.regData1.poke("hFFFFFFFF".U)
      dut.io.DecEx.regData2.poke(1.U)
      dut.clock.step()
      dut.io.ExMem.addr.expect("hFFFFFFFE".U)
    }
  }
}

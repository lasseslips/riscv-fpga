package Alu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.Alu

class SllTest extends AnyFlatSpec with ChiselScalatestTester{
  "SLL" should "Pass" in {
    test(new Alu) { dut =>
      dut.io.DecEx.aluOpcode.poke(2.U)
      dut.io.DecEx.aluSrc.poke("b00".U)

      dut.io.DecEx.regData1.poke(1.U)
      dut.io.DecEx.regData2.poke(2.U)
      dut.io.ExMem.addr.expect(4.U)

      dut.io.DecEx.regData1.poke("hFFFFFFFF".U)
      dut.io.DecEx.regData2.poke(1.U)
      dut.io.ExMem.addr.expect("hFFFFFFFE".U)
    }
  }
}

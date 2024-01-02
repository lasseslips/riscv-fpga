package Alu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.Alu

class AddTest extends AnyFlatSpec with ChiselScalatestTester{
  "ADD" should "Pass" in {
    test(new Alu) { dut =>
      dut.io.control.poke(0.U)
      dut.io.DecEx.regData1.poke(1.U)
      dut.io.DecEx.regData2.poke(2.U)
      dut.io.ExMem.addr.expect(3.U)

      dut.io.DecEx.regData1.poke("hFFFFFFFF".U)
      dut.io.DecEx.regData2.poke(1.U)
      dut.io.ExMem.addr.expect(0.U)
    }
  }
}

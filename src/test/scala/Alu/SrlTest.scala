package Alu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.Alu

class SrlTest extends AnyFlatSpec with ChiselScalatestTester{
  "SRL" should "Pass" in {
    test(new Alu) { dut =>
      dut.io.DecEx.aluOpcode.poke(4.U)
      dut.io.DecEx.aluSrc.poke("b00".U)

      dut.io.DecEx.regData1.poke(1.U)
      dut.io.DecEx.regData2.poke(2.U)
      dut.clock.step()
      dut.io.ExMem.addr.expect(0.U)

      dut.io.DecEx.regData1.poke("hFFFFFFFF".U)
      dut.io.DecEx.regData2.poke(1.U)
      dut.clock.step()
      dut.io.ExMem.addr.expect("h7FFFFFFF".U)
    }
  }
}

package Alu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.Alu

class SltuTest extends AnyFlatSpec with ChiselScalatestTester{
  "SLTU" should "Pass" in {
    test(new Alu) { dut =>
      dut.io.control.poke(9.U)

      dut.io.op1.poke(1.U)
      dut.io.op2.poke(2.U)
      dut.io.res.expect(1.U)

      dut.io.op1.poke("hFFFFFFFF".U)
      dut.io.op2.poke(1.U)
      dut.io.res.expect(0.U)
    }
  }
}

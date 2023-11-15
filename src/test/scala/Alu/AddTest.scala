package Alu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.Alu

class AddTest extends AnyFlatSpec with ChiselScalatestTester{
  "ADD" should "Pass" in {
    test(new Alu) { dut =>
      dut.io.control.poke(0.U)
      dut.io.op1.poke(1.U)
      dut.io.op2.poke(2.U)
      dut.io.res.expect(3.U)

      dut.io.op1.poke("hFFFFFFFF".U)
      dut.io.op2.poke(1.U)
      dut.io.res.expect(0.U)
    }
  }
}

package Alu

import project.Alu
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SrlTest extends AnyFlatSpec with ChiselScalatestTester{
  "SRL" should "Pass" in {
    test(new Alu) { dut =>
      dut.io.control.poke("b100".U)
      for (i <- 0 to (Math.pow(2,5)-1).toInt; j <- 0 to (Math.pow(2,5)-1).toInt) {
        dut.io.op1.poke(i.U)
        dut.io.op2.poke(j.U)
        dut.io.res.expect((i >>> j).U)
      }
      dut.io.op1.poke(0xFFFFFFFFL.U)
      dut.io.op2.poke(0.U)
      dut.io.res.expect((0xFFFFFFFFL).U)
      dut.io.op1.poke(0xFFFFFFFFL.U)
      dut.io.op2.poke(1.U)
      dut.io.res.expect(0x7FFFFFFF.U)
      dut.io.op1.poke(0xFFFFFFFFL.U)
      dut.io.op2.poke(2.U)
      dut.io.res.expect(0x3FFFFFFF.U)
    }
  }
}

package Registers

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.Registers

class RegistersTest extends AnyFlatSpec with ChiselScalatestTester{
  "DataMemory" should "Pass" in {
    test(new Registers) { dut =>
      dut.io.write.poke(false.B)
      dut.io.rs1Idx.poke(0.U)
      dut.io.reg1.expect(0.U)

      dut.io.write.poke(true.B)
      dut.io.wrIdx.poke(6.U)
      dut.io.dataIn.poke("hFE52".U)
      dut.clock.step(1)
      dut.io.write.poke(false.B)
      dut.io.rs2Idx.poke(6.U)
      dut.io.reg2.expect("hFE52".U)


    }
  }
}

package DataPath

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath

class DataPathTest extends AnyFlatSpec with ChiselScalatestTester{
  "DataPath" should "Pass" in {
    test(new DataPath) { dut =>
      //dut.io.instruction.poke("b00000111101100000000000010010011".U)
      dut.io.writeBack.expect(123.U)
    }
  }
}

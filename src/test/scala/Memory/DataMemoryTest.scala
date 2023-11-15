package Memory

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataMemory

class DataMemoryTest extends AnyFlatSpec with ChiselScalatestTester{
  "DataMemory" should "Pass" in {
    test(new DataMemory) { dut =>
      dut.io.write.poke(true.B)
      dut.io.addr.poke("hFF".U)
      dut.io.dataIn.poke(123.U)
      dut.clock.step(1)
      dut.io.enable.poke(true.B)
      dut.clock.step(1)
      dut.io.enable.poke(false.B)
      dut.clock.step(1)
      dut.io.write.poke(false.B)
      dut.clock.step(1)
      dut.io.enable.poke(true.B)
      dut.io.dataOut.expect(123.U)





    }
  }
}

package Memory

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataMemory

class DataMemoryTest extends AnyFlatSpec with ChiselScalatestTester{
  "DataMemory" should "Pass" in {
    test(new DataMemory) { dut =>
      dut.io.addr.poke("hff".U)
      dut.io.write.poke(true.B)

      dut.io.dataIn(0).poke(1.U)
      dut.io.dataIn(1).poke(2.U)
      dut.io.dataIn(2).poke(3.U)
      dut.io.dataIn(3).poke(4.U)

      dut.io.mask(0).poke(true.B)
      dut.io.mask(1).poke(true.B)
      dut.io.mask(2).poke(true.B)
      dut.io.mask(3).poke(true.B)

      dut.clock.step(1)
      dut.io.addr.poke("hff".U)
      dut.io.write.poke(false.B)

      dut.io.mask(0).poke(false.B)
      dut.io.mask(1).poke(false.B)
      dut.io.mask(2).poke(true.B)
      dut.io.mask(3).poke(false.B)

      dut.io.dataOut(0).expect(0.U)
      dut.io.dataOut(1).expect(0.U)
      dut.io.dataOut(2).expect(3.U)
      dut.io.dataOut(3).expect(0.U)
    }
  }
}

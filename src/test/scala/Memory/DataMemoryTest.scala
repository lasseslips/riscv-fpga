package Memory

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.{DataMemory, LoadStoreFunct}

class DataMemoryTest extends AnyFlatSpec with ChiselScalatestTester{
  "DataMemory" should "Pass" in {
    test(new DataMemory) { dut =>
      dut.io.addr.poke("hff".U)
      dut.io.write.poke(true.B)
      dut.io.insType.poke(LoadStoreFunct.LW_SW)
      dut.io.dataIn.poke("hffffffff".U)

      dut.clock.step(1)
      dut.io.addr.poke("hff".U)
      dut.io.write.poke(false.B)
      dut.io.insType.poke(LoadStoreFunct.LB_SB)
      dut.io.dataOut.expect("hFFFFFFFF".U)
      dut.clock.step(1)
      dut.io.addr.poke("hff".U)
      dut.io.write.poke(false.B)
      dut.io.insType.poke(LoadStoreFunct.LBU)
      dut.io.dataOut.expect("hFF".U)
      dut.clock.step(2)

    }
  }
}

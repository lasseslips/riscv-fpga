package Memory

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.lib.LoadStoreFunct
import riscv.stages.DataMemory

class DataMemoryStageTest extends AnyFlatSpec with ChiselScalatestTester{
  "DataMemoryStage" should "Pass" in {
    test(new DataMemory()) { dut =>
      dut.io.ExMem.addr.poke(10.U)
      dut.io.ExMem.data.poke(142.U)
      dut.io.ExMem.memWrite.poke(true.B)
      dut.io.ExMem.memIns.poke(LoadStoreFunct.LW_SW.U)
      dut.clock.step()
      dut.io.ExMem.addr.poke(25.U)
      dut.io.ExMem.data.poke(200.U)
      dut.io.ExMem.memWrite.poke(true.B)
      dut.io.ExMem.memIns.poke(LoadStoreFunct.LW_SW.U)
      dut.clock.step()
      dut.io.ExMem.memWrite.poke(false.B)
      dut.io.ExMem.addr.poke(10.U)
      dut.clock.step()
      dut.io.MemWb.mem.expect(142.U)
      dut.io.ExMem.addr.poke(25.U)
      dut.clock.step()
      dut.io.MemWb.mem.expect(200.U)

    }
  }
}

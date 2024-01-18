package Memory

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.lib.LoadStoreFunct
import riscv.stages.DataMemory

class DataMemoryStageTest extends AnyFlatSpec with ChiselScalatestTester{
  "DataMemoryStage" should "Pass" in {
    test(new DataMemory()) { dut =>
      dut.io.ExMem.addr.poke(9.U)
      dut.io.ExMem.data.poke(142.U)
      dut.io.ExMem.memWrite.poke(true.B)
      dut.io.ExMem.memIns.poke(LoadStoreFunct.LW_SW.U)
      dut.clock.step()
      dut.io.ExMem.addr.poke(24.U)
      dut.io.ExMem.data.poke(200.U)
      dut.io.ExMem.memWrite.poke(true.B)
      dut.io.ExMem.memIns.poke(LoadStoreFunct.LW_SW.U)
      dut.clock.step()
      dut.io.ExMem.memWrite.poke(false.B)
      dut.io.ExMem.addr.poke(8.U)
      dut.clock.step()
      dut.io.MemWb.mem.expect("h8E00".U)
      dut.io.ExMem.addr.poke(24.U)
      dut.clock.step()
      dut.io.MemWb.mem.expect(200.U)
      dut.io.ExMem.addr.poke(17.U)
      dut.io.ExMem.data.poke("hff".U)
      dut.io.ExMem.memIns.poke(LoadStoreFunct.LB_SB.U)
      dut.io.ExMem.memWrite.poke(true.B)
      dut.clock.step()
      dut.io.ExMem.memWrite.poke(false.B)
      dut.io.ExMem.addr.poke(16.U)
      dut.io.ExMem.memIns.poke(LoadStoreFunct.LW_SW.U)
      dut.clock.step()
      dut.io.MemWb.mem.expect("hff00".U)
      dut.clock.step()
      dut.io.ExMem.addr.poke(255.U)
      dut.io.ExMem.data.poke("h1f4".U)
      dut.io.ExMem.memIns.poke(LoadStoreFunct.LW_SW.U)
      dut.io.ExMem.memWrite.poke(true.B)
      dut.clock.step()
      dut.io.ExMem.addr.poke(255.U)
      dut.io.ExMem.memWrite.poke(false.B)
      dut.clock.step()
      dut.io.MemWb.mem.expect("hf4".U)
    }
  }
}

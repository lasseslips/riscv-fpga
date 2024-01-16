package WriteBack

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.stages.{DataMemory, WriteBack}
import riscv.lib.{LoadStoreFunct, RegWriteSrc}

class WriteBackStageTest extends AnyFlatSpec with ChiselScalatestTester{
  "WriteBackStage" should "Pass" in {
    test(new WriteBack()) { dut =>
      dut.io.MemWb.alu.poke(1.U)
      dut.io.MemWb.mem.poke(2.U)
      dut.io.MemWb.pc.poke(3.U)
      dut.io.MemWb.regWriteSrc.poke(RegWriteSrc.ALU.id.U)
      dut.clock.step()
      dut.io.WbDec.wrData.expect(1.U)
      dut.io.MemWb.regWriteSrc.poke(RegWriteSrc.MEMORY.id.U)
      dut.clock.step()
      dut.io.WbDec.wrData.expect(2.U)
      dut.io.MemWb.regWriteSrc.poke(RegWriteSrc.PC.id.U)
      dut.clock.step()
      dut.io.WbDec.wrData.expect((3+4).U)
    }
  }
}

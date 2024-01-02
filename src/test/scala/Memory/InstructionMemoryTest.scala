/*
package Memory

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.InstructionMemory

class InstructionMemoryTest extends AnyFlatSpec with ChiselScalatestTester{
  "InstructionMemory" should "Pass" in {
    test(new InstructionMemory) { dut =>
      dut.io.write.poke(true.B)
      dut.io.dataIn.poke("hffff".U)
      dut.io.addr.poke(0.U)
      dut.clock.step()
      dut.io.write.poke(false.B)
      dut.clock.step(10)
      dut.io.dataOut.expect("hffff".U)
    }
  }
}


 */
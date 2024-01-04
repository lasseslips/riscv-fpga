package Memory

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.{InstructionMemory,Util}

class InstructionMemoryTest extends AnyFlatSpec with ChiselScalatestTester{
  val code = Util.readBin("bin/addneg")
  "InstructionMemory" should "Pass" in {
    test(new InstructionMemory(code)) { dut =>
      dut.io.ExFe.jump.poke(false.B)
      dut.io.halt.poke(false.B)
      dut.io.FeDec.instruction.expect("hfe000513".U)
      dut.clock.step()
      dut.io.ExFe.pc.poke(20.U)
      dut.io.ExFe.jump.poke(true.B)
      dut.clock.step()
      dut.io.FeDec.instruction.expect("h4".U)




    }
  }
}
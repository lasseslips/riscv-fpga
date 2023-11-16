package Decode

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.Decode

class DecodeTest extends AnyFlatSpec with ChiselScalatestTester{
  "DecodeTest" should "Pass" in {
    test(new Decode) { dut =>
      dut.io.instruction.poke("b00000000000100010000000010110011".U)
      dut.io.wrIdx.expect(1.U)
      dut.io.rs1Idx.expect(2.U)
      dut.io.rs2Idx.expect(1.U)
      dut.io.insType.expect(0.U)
      dut.io.instruction.poke("b00000000010000011000000100010011".U)
      dut.io.wrIdx.expect(2.U)
      dut.io.rs1Idx.expect(3.U)
      dut.io.imm.expect(4.U)
      dut.io.insType.expect("h0A".U)
    }
  }
}

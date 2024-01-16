import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class PrintTest extends AnyFlatSpec with ChiselScalatestTester{
  "PRINT" should "Pass" in {
    test(new DataPath("bin/print")) { dut =>
      dut.clock.step(100)
      dut.io.ledOut.expect(5123.U)
      dut.io.sevenSegPins(0).expect("b0110000".U)
      dut.io.sevenSegPins(1).expect("b1000000".U)
      dut.io.sevenSegPins(2).expect("b0011001".U)
      dut.io.sevenSegPins(3).expect("b1111001".U)
      dut.io.sevenSegPins(4).expect("b1000000".U)
      dut.io.sevenSegPins(5).expect("b1000000".U)
      dut.io.sevenSegPins(6).expect("b1000000".U)
      dut.io.sevenSegPins(7).expect("b1000000".U)
    }
  }
}

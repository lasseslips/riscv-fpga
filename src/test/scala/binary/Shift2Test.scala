import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class Shift2Test extends AnyFlatSpec with ChiselScalatestTester{
    "SHIFT2BIN" should "Pass" in {
        test(new DataPath("bin/shift2")) { dut =>
            dut.clock.step(15)
        }
    }
}

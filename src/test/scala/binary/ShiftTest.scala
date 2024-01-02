import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class ShiftTest extends AnyFlatSpec with ChiselScalatestTester{
    "SHIFTBIN" should "Pass" in {
        test(new DataPath("bin/shift")) { dut =>
            dut.clock.step(15)
        }
    }
}

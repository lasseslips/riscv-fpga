import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class BoolTest extends AnyFlatSpec with ChiselScalatestTester{
    "BOOLBIN" should "Pass" in {
        test(new DataPath("bin/bool")) { dut =>
            dut.clock.step(15)
        }
    }
}

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class SetTest extends AnyFlatSpec with ChiselScalatestTester{
    "SETBIN" should "Pass" in {
        test(new DataPath("bin/set")) { dut =>
            dut.clock.step(15)
        }
    }
}

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class BranchcntTest extends AnyFlatSpec with ChiselScalatestTester{
    "BRANCHCNT" should "Pass" in {
        test(new DataPath("bin/blinkingLed")) { dut =>
            dut.clock.step(3000)
        }
    }
}

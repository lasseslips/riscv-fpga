import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath

class AddTest extends AnyFlatSpec with ChiselScalatestTester{
    "ADDPOSBIN" should "Pass" in {
        test(new DataPath("bin/addpos.bin")) { dut =>
            dut.clock.step(10)
        }
    }
}

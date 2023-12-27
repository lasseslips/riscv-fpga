import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class AddnegTest extends AnyFlatSpec with ChiselScalatestTester{
    "ADDNEGBIN" should "Pass" in {
        test(new DataPath("bin/addneg")) { dut =>
            dut.clock.step(15)
        }
    }
}

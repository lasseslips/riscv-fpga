import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class AddposTest extends AnyFlatSpec with ChiselScalatestTester{
    "ADDPOSBIN" should "Pass" in {
        test(new DataPath("bin/addpos")) { dut =>
            dut.clock.step(15)
            dut.io.reg1.expect(5.U)

        }
    }
}

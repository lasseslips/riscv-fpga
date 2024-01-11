import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class GeneralTest extends AnyFlatSpec with ChiselScalatestTester{
  "GeneralTest" should "Pass" in {
    test(new DataPath("bin/ledtest")) { dut =>
      dut.clock.step(200)
    }
  }
}

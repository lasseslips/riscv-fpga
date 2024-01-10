import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class BranchmanyTest extends AnyFlatSpec with ChiselScalatestTester{
    "BRANCHMANY" should "Pass" in {
        test(new DataPath("bin/branchmany")) { dut =>
            dut.clock.step(100)
            dut.io.registers(10).expect(3.U)
            dut.io.registers(11).expect(4.U)
            dut.io.registers(12).expect(3.U)
            dut.io.registers(17).expect(10.U)
        }
    }
}

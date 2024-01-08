import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class AddposTest extends AnyFlatSpec with ChiselScalatestTester{
    "ADDPOSBIN" should "Pass" in {
        test(new DataPath("bin/addpos")) { dut =>
          dut.clock.step(50)
          dut.io.registers(10).expect(5.U)
          dut.io.registers(11).expect(6.U)
          dut.io.registers(12).expect(0.U)
          dut.io.registers(17).expect(10.U)
        }
    }
}

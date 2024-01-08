import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class BranchtrapTest extends AnyFlatSpec with ChiselScalatestTester{
    "BRANCHTRAP" should "Pass" in {
        test(new DataPath("bin/branchtrap")) { dut =>
            dut.clock.step(100)
            dut.io.registers(5).expect("hffffff85".U)
            dut.io.registers(6).expect("hffffff85".U)
            dut.io.registers(7).expect("h7a".U)
            dut.io.registers(12).expect(2.U)
            dut.io.registers(13).expect(3.U)
            dut.io.registers(14).expect(4.U)
            dut.io.registers(15).expect(5.U)
            dut.io.registers(17).expect(10.U)
        }
    }
}

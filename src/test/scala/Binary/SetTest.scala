import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class SetTest extends AnyFlatSpec with ChiselScalatestTester{
    "SETBIN" should "Pass" in {
        test(new DataPath("bin/set")) { dut =>
          dut.clock.step(50)
          dut.io.registers(5).expect("h123".U)
          dut.io.registers(6).expect("h456".U)
          dut.io.registers(12).expect("h123".U)
          dut.io.registers(13).expect("h1".U)
          dut.io.registers(14).expect("h1".U)
          dut.io.registers(16).expect("h1".U)
          dut.io.registers(17).expect("ha".U)
        }
    }
}

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class ShiftTest extends AnyFlatSpec with ChiselScalatestTester{
    "SHIFTBIN" should "Pass" in {
        test(new DataPath("bin/shift")) { dut =>
          dut.clock.step(50)
          dut.io.registers(11).expect("h1fe".U)
          dut.io.registers(12).expect("hff".U)
          dut.io.registers(17).expect("ha".U)
        }
    }
}

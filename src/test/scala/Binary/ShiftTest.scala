import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class ShiftTest extends AnyFlatSpec with ChiselScalatestTester{
    "SHIFTBIN" should "Pass" in {
        test(new DataPath("bin/shift")) { dut =>
          dut.clock.step(50)
          dut.io.registers(10).expect("hffff0000".U)
          dut.io.registers(11).expect("h0000ffff".U)
          dut.io.registers(12).expect("hffffffff".U)
          dut.io.registers(17).expect("ha".U)
        }
    }
}

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class StringTest extends AnyFlatSpec with ChiselScalatestTester{
    "STRING" should "Pass" in {
        test(new DataPath("bin/string")) { dut =>
          dut.clock.step(100)
          dut.io.registers(5).expect("h20".U)
          dut.io.registers(6).expect("h0".U)
          dut.io.registers(7).expect("h69".U)
          dut.io.registers(8).expect("h10".U)
          dut.io.registers(9).expect("h74".U)
          dut.io.registers(10).expect("he".U)
          dut.io.registers(11).expect("h13".U)
          dut.io.registers(12).expect("h20".U)
          dut.io.registers(13).expect("h5".U)
          dut.io.registers(14).expect("h6d".U)
          dut.io.registers(15).expect("h5".U)
          dut.io.registers(16).expect("h65".U)
          dut.io.registers(17).expect("ha".U)
          dut.io.registers(18).expect("h0".U)
          dut.io.registers(19).expect("h20".U)
          dut.io.registers(20).expect("hffffff83".U)
          dut.io.registers(21).expect("h79".U)
          dut.io.registers(22).expect("h0".U)
          dut.io.registers(23).expect("h6f".U)
          dut.io.registers(24).expect("h5".U)
          dut.io.registers(25).expect("h75".U)
          dut.io.registers(26).expect("h0".U)
          dut.io.registers(27).expect("h27".U)
          dut.io.registers(28).expect("h13".U)
          dut.io.registers(29).expect("h72".U)
          dut.io.registers(30).expect("h5".U)
          dut.io.registers(31).expect("h65".U)
        }
    }
}

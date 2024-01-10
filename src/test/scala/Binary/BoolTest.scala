import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class BoolTest extends AnyFlatSpec with ChiselScalatestTester{
    "BOOLBIN" should "Pass" in {
        test(new DataPath("bin/bool")) { dut =>
          dut.clock.step(50)
          dut.io.registers(5).expect("h12345678".U)
          dut.io.registers(6).expect("habcdefab".U)
          dut.io.registers(11).expect("hb9f9b9d3".U)
          dut.io.registers(12).expect("hbbfdfffb".U)
          dut.io.registers(13).expect("h02044628".U)
          dut.io.registers(14).expect("h12345359".U)
          dut.io.registers(15).expect("h12345779".U)
          dut.io.registers(16).expect("h420".U)
          dut.io.registers(17).expect("ha".U)
        }
    }
}

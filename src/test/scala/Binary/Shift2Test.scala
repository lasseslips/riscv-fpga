import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class Shift2Test extends AnyFlatSpec with ChiselScalatestTester{
    "SHIFT2BIN" should "Pass" in {
        test(new DataPath("bin/shift2")) { dut =>
          dut.clock.step(50)
          dut.io.registers(5).expect(1.U)
          dut.io.registers(10).expect("habcdefab".U)
          dut.io.registers(11).expect("h579bdf56".U)
          dut.io.registers(12).expect("h55e6f7d5".U)
          dut.io.registers(13).expect("hd5e6f7d5".U)
          dut.io.registers(14).expect("h5e6f7d58".U)
          dut.io.registers(15).expect("h1579bdf5".U)
          dut.io.registers(16).expect("hf579bdf5".U)
          dut.io.registers(17).expect("ha".U)
        }
    }
}

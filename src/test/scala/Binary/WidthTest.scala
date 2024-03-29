import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class WidthTest extends AnyFlatSpec with ChiselScalatestTester{
    "WIDTH" should "Pass" in {
      //uses unaligned memory access which is not supported in my build.
        test(new DataPath("bin/width")) { dut =>
          dut.clock.step(100)
          dut.io.registers(5).expect("habcdefab".U)
          dut.io.registers(6).expect("h7f".U)
          dut.io.registers(11).expect("hffffffab".U)
          dut.io.registers(12).expect("hffffefab".U)
          dut.io.registers(13).expect("hefab".U)
          dut.io.registers(14).expect("hab".U)
          dut.io.registers(15).expect("hefab".U)
          dut.io.registers(16).expect("hab".U)
          dut.io.registers(17).expect("ha".U)
        }
    }
}

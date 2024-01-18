import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class LoadstoreTest extends AnyFlatSpec with ChiselScalatestTester{
    "LOADSTORE" should "Pass" in {
        test(new DataPath("bin/loadstore")) { dut =>
          dut.clock.step(100)
          dut.io.registers(10).expect("h1f4".U)
          dut.io.registers(11).expect("hff".U)
          dut.io.registers(13).expect("h0".U)
          dut.io.registers(14).expect("hf4".U)
          dut.io.registers(17).expect(10.U)

          dut.io.registers(5).expect("hefab".U)
          dut.io.registers(6).expect("hab".U)
          dut.io.registers(7).expect("hefab".U)

          dut.io.registers(24).expect("hfffffff4".U)
          dut.io.registers(25).expect("hffffefab".U)
          dut.io.registers(26).expect("hab".U)
          dut.io.registers(27).expect("hefab".U)
        }
    }
}

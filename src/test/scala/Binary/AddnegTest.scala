import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv._

class AddnegTest extends AnyFlatSpec with ChiselScalatestTester{
    "ADDNEGBIN" should "Pass" in {
        test(new DataPath("bin/addneg")) { dut =>
            dut.clock.step(20)
            dut.io.registers(10).expect("hffffffe0".U)
            dut.io.registers(11).expect("hffffffc0".U)
            dut.io.registers(12).expect("h0".U)
            dut.io.registers(17).expect(10.U)
        }
    }
}

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv.InstructionMemory

class AddlargeTest extends AnyFlatSpec with ChiselScalatestTester{
    "ADDLARGEBIN" should "Pass" in {
        test(new DataPath("bin/addlarge")) { dut =>
          dut.clock.step(50)
          dut.io.registers(10).expect(1.U)
          dut.io.registers(11).expect("hfffffffe".U)
          dut.io.registers(12).expect(1.U)
          dut.io.registers(17).expect(10.U)

        }
    }
}

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv.InstructionMemory

class AddlargeTest extends AnyFlatSpec with ChiselScalatestTester{
    "ADDLARGEBIN" should "Pass" in {
        test(new DataPath("bin/addlarge")) { dut =>
          dut.clock.step(50)
          dut.io.registers(10).expect("h80000001".U)
          dut.io.registers(11).expect("h7ffffffe".U)
          dut.io.registers(12).expect("hffffffff".U)
          dut.io.registers(17).expect(10.U)

        }
    }
}

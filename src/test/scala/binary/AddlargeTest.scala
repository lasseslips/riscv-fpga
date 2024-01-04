import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.DataPath
import riscv.InstructionMemory

class AddlargeTest extends AnyFlatSpec with ChiselScalatestTester{
    "ADDLARGEBIN" should "Pass" in {
        test(new DataPath("bin/addlarge")) { dut =>
        }
    }
}

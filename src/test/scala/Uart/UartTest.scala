package Uart

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import peripherals.Uart

class UartTest extends AnyFlatSpec with ChiselScalatestTester{
  "UART" should "Pass" in {
    test(new Uart) { dut =>
      dut.clock.step(500)

    }
  }
}

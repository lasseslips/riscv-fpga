package peripherals

import chisel3._
import chisel.lib.uart._

class Uart extends Module {
  val io = IO(new Bundle() {
  })
  val FREQ = 20000000


  val uart = Module(new BufferedTx(115200,FREQ))
  uart.io.channel.valid := true.B
  uart.io.channel.bits := '0'.U
}


object UartTest extends App {
  println("Generating uart verilog")
  emitVerilog(new Uart, Array("--target-dir", "generated"))
}

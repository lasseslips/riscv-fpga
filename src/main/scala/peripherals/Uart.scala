package peripherals

import chisel3._
import chisel3.util._
import chisel.lib.uart._

class Uart extends Module {
  val io = IO(new Bundle() {
    val uartTx = Output(UInt(1.W))
    val uartRx = Input(UInt(1.W))
  })
  val FREQ = 50000000

  

  val uart = Module(new BufferedTx(FREQ,9600))
  //val hello = VecInit('H'.U, 'e'.U, 'l'.U, 'l'.U, 'o'.U)


  val cntReg = RegInit(0.U(32.W))
  val validReg = RegInit(0.U(1.W))
  when(cntReg === (FREQ/2).U) {
    cntReg := 0.U
    validReg := ~validReg
    } .otherwise {
      cntReg := cntReg + 1.U
    }
  
    when(!uart.io.channel.ready) {
      validReg := 0.U
      uart.io.channel.valid := 0.U
      cntReg := 0.U
    }

  uart.io.channel.bits := "h4d".U//hello(cntReg)
  uart.io.channel.valid := validReg


  

  
  /*
  when(uart.io.channel.ready) {
    uart.io.channel.valid := true.B
    cntReg := cntReg + 1.U
    } .otherwise {
    uart.io.channel.valid := false.B
    }
    */
  io.uartTx := uart.io.txd
  //io.uartTx := io.uartRx
  



}


object UartTest extends App {
  println("Generating uart verilog")
  emitVerilog(new Uart, Array("--target-dir", "generated"))
}

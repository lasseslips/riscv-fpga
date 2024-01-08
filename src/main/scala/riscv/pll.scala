import chisel3._
import chisel3.util.HasBlackBoxResource

class BlackBoxRealAdd extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val areset = Input(Bool())
    val inclk0 = Input(Clock())
    val c0 = Output(Clock())
    val locked = Output(Bool())
  })
  addResource("quartus/pll(pll.v)")
}

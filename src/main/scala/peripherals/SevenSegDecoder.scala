package peripherals

import chisel3._
import chisel3.util._

class SevenSegDecoder extends Module {
  val io = IO(new Bundle {
    val sw = Input(UInt(4.W))
    val seg = Output(UInt(7.W))
  })

  val sevSeg = WireInit(0.U(7.W))

  io.seg := ~sevSeg
  switch(io.sw) {
    is ("h0".U) {sevSeg := "b0111111".U}
    is ("h1".U) {sevSeg := "b0000110".U}
    is ("h2".U) {sevSeg := "b1011011".U}
    is ("h3".U) {sevSeg := "b1001111".U}
    is ("h4".U) {sevSeg := "b1100110".U}
    is ("h5".U) {sevSeg := "b1101101".U}
    is ("h6".U) {sevSeg := "b1111101".U}
    is ("h7".U) {sevSeg := "b0000111".U}
    is ("h8".U) {sevSeg := "b1111111".U}
    is ("h9".U) {sevSeg := "b1101111".U}
    is ("ha".U) {sevSeg := "b1110111".U}
    is ("hb".U) {sevSeg := "b1111100".U}
    is ("hc".U) {sevSeg := "b0111001".U}
    is ("hd".U) {sevSeg := "b1011110".U}
    is ("he".U) {sevSeg := "b1111001".U}
    is ("hf".U) {sevSeg := "b1110001".U}
  }

}



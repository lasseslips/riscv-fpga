package riscv

import chisel3._
import chisel3.util._
class Branch extends Module {
  val io = IO(new Bundle() {
    val rs1 = Input(UInt(32.W))
    val rs2 = Input(UInt(32.W))
    val branchEnable = Input(Bool())
    val branchType = Input(UInt(3.W))
    val branching = Output(Bool())
  })

  val isCondTrue = Wire(Bool())
  isCondTrue := false.B
  switch(io.branchType) {
    is(BranchFunct.BEQ.U) {
     when(io.rs1.asSInt === io.rs2.asSInt) {
       isCondTrue := true.B
     }
    }
    is(BranchFunct.BNE.U) {
      when(io.rs1.asSInt =/= io.rs2.asSInt) {
        isCondTrue := true.B
      }
    }
    is(BranchFunct.BLT.U) {
      when(io.rs1.asSInt < io.rs2.asSInt) {
        isCondTrue := true.B
      }
    }
    is(BranchFunct.BGE.U) {
      when(io.rs1.asSInt >= io.rs2.asSInt) {
        isCondTrue := true.B
      }
    }
    is(BranchFunct.BLTU.U) {
      when(io.rs1 < io.rs2) {
        isCondTrue := true.B
      }
    }
    is(BranchFunct.BGEU.U) {
      when(io.rs1 >= io.rs2) {
        isCondTrue := true.B
      }
    }
  }
   io.branching := isCondTrue & io.branchEnable

}

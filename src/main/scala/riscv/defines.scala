package riscv

object Opcode {
  val AluImm = 0x13
  val Alu = 0x33
  val Branch = 0x63
  val Load = 0x03
  val Store = 0x23
  val Lui = 0x37
  val AuiPc = 0x17
  val Jal = 0x6f
  val JalR = 0x67
  val Fence = 0x0f
  val ECall = 0x73
}

object AluType extends Enumeration {
  type AluType = Value
  val ADD, SUB, SLL, XOR, SRL, SRA, OR, AND, SLT, SLTU = Value
}

object insType {
  val ADD = 0x00
  val SUB = 0x01
  val SLL = 0x02
  val SLT = 0x03
  val SLTU = 0x04
  val XOR = 0x05
  val SRL = 0x06
  val SRA = 0x07
  val OR = 0x08
  val AND = 0x09
  val ADDI = 0x0A
  val SLTI = 0x0B
  val SLTIU = 0x0C
  val XORI = 0x0D
  val ORI = 0x0E
  val ANDI = 0x0F
  val SLLI = 0x10
  val SRLI = 0x11
  val SRAI = 0x12
  val BEQ = 0x13
  val BNE = 0x14
  val BLT = 0x15
  val BGE = 0x16
  val BLTU = 0x17
  val BGEU = 0x18
  val LB = 0x19
  val LH = 0x1A
  val LW = 0x1B
  val LBU = 0x1C
  val LHU = 0x1D
  val SB = 0x1E
  val SH = 0x1F
  val SW = 0x20
  val LUI = 0x21
  val AUIPC = 0x22
  val JAL = 0x23
  val JALR = 0x24
  val FENCE = 0x25
  val ECALL = 0x26
  val EBREAK = 0x27
  val CSRRW = 0x28
  val CSRRS = 0x29
  val CSRRC = 0x2A
  val CSRRWI = 0x2B
  val CSRRSI = 0x2C
  val CSRRCI = 0x2D
}

object AluFunct3 {
  val ADD_SUB = 0x00 // no SUB in I-type
  val SLL = 0x01
  val SLT = 0x02
  val SLTU = 0x03
  val XOR = 0x04
  val SRL_SRA = 0x05
  val OR = 0x06
  val AND = 0x07
}
object AluImmFunct3 {
  val ADDI = 0x00
  val SLLI = 0x01
  val SLTI = 0x02
  val SLTIU = 0x03
  val XORI = 0x04
  val SRLI_SRAI = 0x05
  val ORI = 0x06
  val ANDI = 0x07
}

object BranchFunct {
  val BEQ = 0x00
  val BNE = 0x01
  val BLT = 0x04
  val BGE = 0x05
  val BLTU = 0x06
  val BGEU = 0x07
}

object LoadFunct {
  val LB = 0x00
  val LH = 0x01
  val LW = 0x02
  val LBU = 0x04
  val LHU = 0x05
}
object StoreFunct {
  val SB = 0x00
  val SH = 0x01
  val SW = 0x02
}

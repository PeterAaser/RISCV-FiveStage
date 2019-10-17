package FiveStage
import cats.implicits._
import fileUtils._

import Data._
import PrintUtils._

object Ops {
  
  sealed trait Op extends RegLayout

  sealed trait RegLayout
  sealed trait RType extends RegLayout { def rd: Reg; def rs1: Reg; def rs2: Reg }
  sealed trait IType extends RegLayout { def rd: Reg; def rs1: Reg;              }
  sealed trait SType extends RegLayout {              def rs1: Reg; def rs2: Reg }
  sealed trait UType extends RegLayout { def rd: Reg;                            }

  sealed trait ImmType
  sealed trait NoImmediate    extends ImmType
  sealed trait IImmediate     extends ImmType
  sealed trait SImmediate     extends ImmType
  sealed trait BImmediate     extends ImmType
  sealed trait UImmediate     extends ImmType
  sealed trait JImmediate     extends ImmType
  sealed trait ShiftImmediate extends ImmType

  sealed trait Comparison {
    def run(rs1Val: Int, rs2Val: Int): Boolean
  }
  case object EQ  extends Comparison { def run(rs1Val: Int, rs2Val: Int): Boolean = rs1Val == rs2Val }
  case object NE  extends Comparison { def run(rs1Val: Int, rs2Val: Int): Boolean = rs1Val != rs2Val }
  case object GE  extends Comparison { def run(rs1Val: Int, rs2Val: Int): Boolean = rs1Val >= rs2Val }
  case object LT  extends Comparison { def run(rs1Val: Int, rs2Val: Int): Boolean = rs1Val <  rs2Val }
  case object GEU extends Comparison { def run(rs1Val: Int, rs2Val: Int): Boolean = !(rs1Val `u>` rs2Val) }
  case object LTU extends Comparison { def run(rs1Val: Int, rs2Val: Int): Boolean = rs1Val `u>` rs2Val }

  case class Branch(rs1: Reg, rs2: Reg, dst: Label, comp: Comparison) extends Op with SType
  object Branch{
    def beq( rs1: Int, rs2: Int, dst: Label) = Branch(Reg(rs1), Reg(rs2), dst, EQ)
    def bne( rs1: Int, rs2: Int, dst: Label) = Branch(Reg(rs1), Reg(rs2), dst, NE)
    def blt( rs1: Int, rs2: Int, dst: Label) = Branch(Reg(rs1), Reg(rs2), dst, LT)
    def bge( rs1: Int, rs2: Int, dst: Label) = Branch(Reg(rs1), Reg(rs2), dst, GE)
    def bltu(rs1: Int, rs2: Int, dst: Label) = Branch(Reg(rs1), Reg(rs2), dst, LTU)
    def bgeu(rs1: Int, rs2: Int, dst: Label) = Branch(Reg(rs1), Reg(rs2), dst, GEU)

    def ble( rs1: Int, rs2: Int, dst: Label) = Branch(Reg(rs2), Reg(rs1), dst, GE)
    def bgt( rs1: Int, rs2: Int, dst: Label) = Branch(Reg(rs2), Reg(rs1), dst, LT)
    def bleu(rs1: Int, rs2: Int, dst: Label) = Branch(Reg(rs2), Reg(rs1), dst, GEU)
    def bgtu(rs1: Int, rs2: Int, dst: Label) = Branch(Reg(rs2), Reg(rs1), dst, LTU)

    def beqz(rs1: Int, dst: Label) = Branch(Reg(rs1), Reg(0), dst, EQ)
    def bnez(rs1: Int, dst: Label) = Branch(Reg(rs1), Reg(0), dst, NE)
    def blez(rs1: Int, dst: Label) = Branch(Reg(0), Reg(rs1), dst, GE)
    def bgez(rs1: Int, dst: Label) = Branch(Reg(rs1), Reg(0), dst, GE)
    def bltz(rs1: Int, dst: Label) = Branch(Reg(rs1), Reg(0), dst, LT)
    def bgtz(rs1: Int, dst: Label) = Branch(Reg(0), Reg(rs1), dst, LT)
  }

  sealed trait someDecorator
  sealed trait ArithOp {
    def run(operand1: Int, operand2: Int): Int
  }
  case object ADD  extends ArithOp { def run(operand1: Int, operand2: Int): Int = operand1 + operand2 }
  case object SUB  extends ArithOp { def run(operand1: Int, operand2: Int): Int = operand1 - operand2 }
  case object OR   extends ArithOp { def run(operand1: Int, operand2: Int): Int = operand1 | operand2 }
  case object XOR  extends ArithOp { def run(operand1: Int, operand2: Int): Int = operand1 ^ operand2 }
  case object AND  extends ArithOp { def run(operand1: Int, operand2: Int): Int = operand1 & operand2 }
  case object SLL  extends ArithOp { def run(operand1: Int, operand2: Int): Int = operand1 << operand2 }
  case object SRL  extends ArithOp { def run(operand1: Int, operand2: Int): Int = operand1 >>> operand2 }
  case object SRA  extends ArithOp { def run(operand1: Int, operand2: Int): Int = operand1 >> operand2 }
  case object SLT  extends ArithOp { def run(operand1: Int, operand2: Int): Int = if(operand2 > operand1) 1 else 0 }
  case object SLTU extends ArithOp { def run(operand1: Int, operand2: Int): Int = if(operand2 `u>` operand1) 1 else 0 }

  case class Arith(rd: Reg, rs1: Reg, rs2: Reg, op: ArithOp) extends Op with RType
  object Arith {
    def add( rd: Int, rs1: Int, rs2: Int) = Arith(Reg(rd), Reg(rs1), Reg(rs2), ADD)
    def sub( rd: Int, rs1: Int, rs2: Int) = Arith(Reg(rd), Reg(rs1), Reg(rs2), SUB)
    def or(  rd: Int, rs1: Int, rs2: Int) = Arith(Reg(rd), Reg(rs1), Reg(rs2), OR)
    def xor( rd: Int, rs1: Int, rs2: Int) = Arith(Reg(rd), Reg(rs1), Reg(rs2), XOR)
    def and( rd: Int, rs1: Int, rs2: Int) = Arith(Reg(rd), Reg(rs1), Reg(rs2), AND)
    def sll( rd: Int, rs1: Int, rs2: Int) = Arith(Reg(rd), Reg(rs1), Reg(rs2), SLL)
    def srl( rd: Int, rs1: Int, rs2: Int) = Arith(Reg(rd), Reg(rs1), Reg(rs2), SRL)
    def sra( rd: Int, rs1: Int, rs2: Int) = Arith(Reg(rd), Reg(rs1), Reg(rs2), SRA)
    def slt( rd: Int, rs1: Int, rs2: Int) = Arith(Reg(rd), Reg(rs1), Reg(rs2), SLT)
    def sltu(rd: Int, rs1: Int, rs2: Int) = Arith(Reg(rd), Reg(rs1), Reg(rs2), SLTU)
  }

  def NOP = ArithImm.nop



  case class ArithImm(rd: Reg, rs1: Reg, imm: Imm, op: ArithOp) extends Op with IType
  object ArithImm {
    def add( rd: Int, rs1: Int, imm: Int) = ArithImm(Reg(rd), Reg(rs1), Imm(imm), ADD)
    def or(  rd: Int, rs1: Int, imm: Int) = ArithImm(Reg(rd), Reg(rs1), Imm(imm), OR)
    def xor( rd: Int, rs1: Int, imm: Int) = ArithImm(Reg(rd), Reg(rs1), Imm(imm), XOR)
    def and( rd: Int, rs1: Int, imm: Int) = ArithImm(Reg(rd), Reg(rs1), Imm(imm), AND)
    def slt( rd: Int, rs1: Int, imm: Int) = ArithImm(Reg(rd), Reg(rs1), Imm(imm), SLT)
    def sltu(rd: Int, rs1: Int, imm: Int) = ArithImm(Reg(rd), Reg(rs1), Imm(imm), SLTU)
    def nop = add(0, 0, 0)
  }

  case class ArithImmShift(rd: Reg, rs1: Reg, shamt: Imm, op: ArithOp) extends Op with IType
  object ArithImmShift {
    def sll( rd: Int, rs1: Int, imm: Int) = ArithImmShift(Reg(rd), Reg(rs1), Imm(imm), SLL)
    def srl( rd: Int, rs1: Int, imm: Int) = ArithImmShift(Reg(rd), Reg(rs1), Imm(imm), SRL)
    def sra( rd: Int, rs1: Int, imm: Int) = ArithImmShift(Reg(rd), Reg(rs1), Imm(imm), SRA)
  }

  case class LUI(rd: Reg, imm: Imm)   extends Op with UType
  case class AUIPC(rd: Reg, imm: Imm) extends Op with UType

  case class JALR(rd: Reg, rs1: Reg, dst: String) extends Op with IType
  case class JAL(rd: Reg, dst: String) extends Op with UType
  case class SW(rs2: Reg, rs1: Reg, offset: Imm) extends Op with SType
  case class LW(rd: Reg, rs1: Reg, offset: Imm)  extends Op with IType


  object LUI { def apply(rd: Int, imm: Int): LUI = LUI(Reg(rd), Imm(imm)) }
  object AUIPC { def apply(rd: Int, imm: Int): AUIPC = AUIPC(Reg(rd), Imm(imm)) }

  object JAL{ def apply(rd: Int, dst: String): JAL = JAL(Reg(rd), dst) }
  object JALR{ def apply(rd: Int, rs1: Int, dst: String): JALR = JALR(Reg(rd), Reg(rs1), dst) }
  object SW  { def apply(rs2: Int, rs1: Int, offset: Int): SW = SW(Reg(rs2), Reg(rs1), Imm(offset)) }
  object LW  { def apply(rd: Int, rs1: Int, offset: Int): LW = LW(Reg(rd), Reg(rs1), Imm(offset)) }

  // This op should not be assembled, but will for the sake of simplicity be rendered as a NOP
  case object DONE extends Op with IType { val rd = Reg(0); val rs1 = Reg(0) }
}

package FiveStage
import cats.implicits._

import Data._
import Ops._
import fileUtils._
import PrintUtils._

object assembler {

  type InstructionFragment = Either[(String, Addr), Int]

  /**
    * Will only be called by applyImmedate, thus error propagation is not needed
    * Kind of a kludge, but it works, don't touch!
    */
  def setField(firstBit: Int, size: Int, field: Int): Int => Int = instruction => {
    val shiftedField = field << firstBit
    val mask  = (1 << size) - 1
    val shiftedMask = (mask << firstBit)
    val masked = ((~instruction) | shiftedMask)
    val maskedInv = ~(masked)

    val ret = (shiftedField & shiftedMask) | maskedInv

    ret
  }


  def getSubField(firstBit: Int, size: Int): Int => Int = word => {
    val bitsLeft = 32 - firstBit
    val bitsRight = 32 - size
    val leftShifted = word << bitsLeft
    val rightShifted = leftShifted >> bitsRight
    rightShifted
  }


  /**
    Splits the immediate value into fields given by points.
    The order of points is important!
    points is of type idx, size
    */
  def applyImmediate(immediateBits: Int, immediate: Int, points: List[(Int, Int)]): Int => Int = instruction => {

    def go(instruction: Int, immediateIndex: Int, points: List[(Int,Int)]): Int = points match {
      case h :: t => {
        val (immFirstBit, size) = h
        val firstBit = (immFirstBit - size) + 1
        val immSubField = getSubField(immediateIndex, size)(immediate)
        val nextImmIndex = immediateIndex - size
        val nextInstruction = setField(firstBit, size, immSubField)(instruction)
        go(nextInstruction, nextImmIndex, points.tail)
      }
      case _ => {
        instruction
      }
    }

    go(instruction, immediateBits, points)
  }


  def applyImmediateU(immediate: Int, points: List[(Int, Int)], addr: Addr): Int => InstructionFragment = instruction => {
    def totalBits = points.foldLeft(0){ case(acc, (first, size)) => acc + size }
    totalBits.nBitsU.toRight("Negative number used as unsigned immediate", addr).flatMap { bits =>
      Either.cond(bits < totalBits, applyImmediate(totalBits, immediate, points)(instruction), ("Immediate unsigned too large", addr))
    }
  }

  def applyImmediateS(immediate: Int, points: List[(Int, Int)], addr: Addr): Int => InstructionFragment = instruction => {
    def totalBits = points.foldLeft(0){ case(acc, (first, size)) => acc + size }
    if(totalBits < immediate.nBitsS) Left((s"Immediate signed too large. immedate: $immediate, immediate.nBitsS = ${immediate.nBitsS}, total bits: $totalBits", addr))
    else Right(applyImmediate(totalBits, immediate, points)(instruction))
  }


  /**
    * Used by JALR, LW and arithmetic immediate ops.
    * JALR is sort of the odd man out here as it should be unsigned.
    * This issue should not surface at the very limited address space
    * for your design. (I hope)
    */
  def setItypeImmediate(immediate: Int, addr: Addr): Int => InstructionFragment = {
    val points = List((31, 12))
    val withField = applyImmediateS(immediate, points, addr)
    withField
  }

  /**
    * Used by SW
    */
  def setStypeImmediate(immediate: Int, addr: Addr): Int => InstructionFragment = {
    val points = List((31, 7), (11, 5))
    val withField = applyImmediateS(immediate, points, addr)
    withField
  }

  /**
    * Used by Branches. PC relative jump, thus signed
    * Last bit is not used, hence the shift
    */
  def setBtypeImmediate(immediate: Int, addr: Addr): Int => InstructionFragment = {
    val points = List((31, 1), (7, 1), (30, 6), (11, 4))
    val withField = applyImmediateS((immediate >> 1), points, addr)
    withField
  }

  /**
    * Used by LUI and AUIPC. Unsigned
    */
  def setUtypeImmediate(immediate: Int, addr: Addr): Int => InstructionFragment = {
    val points = List((31, 20))
    val withField = applyImmediateU(immediate, points, addr)
    withField
  }

  /**
    * Used by JAL. PC relative jump, thus signed
    * The last bit is not used, hence the shift
    */
  def setJtypeImmediate(immediate: Int, addr: Addr): Int => InstructionFragment = {
    val points = List((31, 1), (19, 8), (20, 1), (30, 10))
    applyImmediateU((immediate >> 1), points, addr)
  }

  /**
    * Used by SRI, SRAI, SLLI
    */
  def setShiftTypeImmediate(shamt: Int, addr: Addr): Int => InstructionFragment = {
    val points = List((24, 5))
    applyImmediateU(shamt, points, addr)
  }

  def setOpCode(opcode: Int): Int => Int = setField(0, 7, opcode)
  def setFunct7(funct7: Int): Int => Int = setField(25, 7, funct7)
  def setFunct3(funct3: Int): Int => Int = setField(12, 3, funct3)
  def setRs1(rs1: Int): Int => Int = setField(15, 5, rs1)
  def setRs2(rs2: Int): Int => Int = setField(20, 5, rs2)
  def setRd(rd: Int): Int => Int = setField(7, 5, rd)


  def setOpCode(op: Op): Int => Int = op match {
    case x: Branch        => setOpCode("1100011".binary)
    case x: Arith         => setOpCode("0110011".binary)
    case x: ArithImm      => setOpCode("0010011".binary)
    case x: ArithImmShift => setOpCode("0010011".binary)
    case x: LW            => setOpCode("0000011".binary)
    case x: SW            => setOpCode("0100011".binary)
    case x: JALR          => setOpCode("1100111".binary)
    case x: JAL           => setOpCode("1101111".binary)
    case x: AUIPC         => setOpCode("0010111".binary)
    case x: LUI           => setOpCode("0110111".binary)
    case DONE             => setOpCode("0010011".binary) // done is turned into a NOP in the assembler.
  }

  def setComparisonFunct(cmp: Comparison): Int => Int = cmp match {
    case EQ  => setFunct3("000".binary)
    case NE  => setFunct3("001".binary)
    case GE  => setFunct3("101".binary)
    case LT  => setFunct3("100".binary)
    case GEU => setFunct3("111".binary)
    case LTU => setFunct3("110".binary) 
  }

  def setBranchDestination(labelMap: Map[Label, Addr], op: Branch, opAddr: Addr): Int => InstructionFragment = instruction => {
    labelMap.lift(op.dst).toRight((s"destination ${op.dst} not found", opAddr)).flatMap{ dstAddr =>
      setBtypeImmediate(dstAddr.value - opAddr.value, opAddr)(instruction)
    }
  }

  def setArithFunct(op: ArithOp): Int => Int = op match {
    case ADD  => setFunct7("0000000".binary) andThen setFunct3("000".binary)
    case SUB  => setFunct7("0100000".binary) andThen setFunct3("000".binary)
    case SLL  => setFunct7("0000000".binary) andThen setFunct3("001".binary)
    case SLT  => setFunct7("0000000".binary) andThen setFunct3("010".binary)
    case SLTU => setFunct7("0000000".binary) andThen setFunct3("011".binary)
    case XOR  => setFunct7("0000000".binary) andThen setFunct3("100".binary)
    case SRL  => setFunct7("0000000".binary) andThen setFunct3("101".binary)
    case SRA  => setFunct7("0100000".binary) andThen setFunct3("101".binary)
    case OR   => setFunct7("0000000".binary) andThen setFunct3("110".binary)
    case AND  => setFunct7("0000000".binary) andThen setFunct3("111".binary)
  }


  def assembleRegLayout(op: RegLayout): Int => Int = {

    def assembleRType(op: RType): Int => Int =
      setRd(op.rd.value) andThen
      setRs1(op.rs1.value) andThen
      setRs2(op.rs2.value)

    def assembleIType(op: IType): Int => Int =
      setRd(op.rd.value) andThen
      setRs1(op.rs1.value)

    def assembleSType(op: SType): Int => Int = {
      instruction =>
      (setRs1(op.rs1.value) andThen
      setRs2(op.rs2.value))(instruction)
    }

    def assembleUType(op: UType): Int => Int =
      setRd(op.rd.value)

    op match {
      case op: RType => assembleRType(op)
      case op: IType => assembleIType(op)
      case op: SType => assembleSType(op)
      case op: UType => assembleUType(op)
    }
  }


  def assembleImmediate(op: Op, addr: Addr, labelMap: Map[Label, Addr]): Int => Either[(String, Addr), Int] = op match {
    case DONE              => instruction => Right(instruction)
    case op: Arith         => instruction => Right(instruction)
    case op: ArithImm      => setItypeImmediate(op.imm.value, addr)
    case op: ArithImmShift => setShiftTypeImmediate(op.shamt.value, addr)
    case op: Branch        => setBranchDestination(labelMap, op, addr)
    case op: JALR          => instruction => labelMap.lift(op.dst).toRight(s"label ${op.dst} not found", addr).flatMap(addr => setItypeImmediate(addr.value, addr)(instruction))
    case op: AUIPC         => setUtypeImmediate(op.imm.value, addr)
    case op: LUI           => setUtypeImmediate(op.imm.value, addr)
    case op: LW            => setItypeImmediate(op.offset.value, addr)
    case op: SW            => setStypeImmediate(op.offset.value, addr)
    case op: JAL           => instruction => {
      val addressDistance = labelMap
        .lift(op.dst).toRight(s"label ${op.dst} not found", addr)
        .map(absoluteAddr => absoluteAddr - addr)

      import PrintUtils._

      addressDistance.flatMap(distance =>
        setJtypeImmediate(distance.value, addr)(instruction))
    }
  }


  def assembleOp(
    op       : Op with RegLayout,
    opAddr   : Addr,
    labelMap : Map[Label, Addr]): Either[(String, Addr), Int] = {

    val layout    = assembleRegLayout(op)
    val immediate = assembleImmediate(op, opAddr, labelMap)
    val opcode    = setOpCode(op)

    val extras: Int => Int = (instruction: Int) => op match {
      case op: Branch        => setComparisonFunct(op.comp)(instruction)
      case op: ArithImm      => setArithFunct(op.op)(instruction)
      case op: ArithImmShift => setArithFunct(op.op)(instruction)
      case op: Arith         => setArithFunct(op.op)(instruction)
      case op: JALR          => setFunct3("000".binary)(instruction)
      case op: LW            => setFunct3("010".binary)(instruction)
      case op: SW            => setFunct3("010".binary)(instruction)
      case DONE              => (setFunct3("000".binary) andThen setFunct7("0000000".binary))(instruction)

      case op: AUIPC         => instruction
      case op: JAL           => instruction
      case op: LUI           => instruction
    }

    val withOp = opcode(0)
    val withLayout = layout(withOp)
    val withImmediates = immediate(withLayout)
    val withExtras = withImmediates.map(extras)


    val finalOp = (opcode andThen layout andThen extras andThen immediate)(0)

    finalOp
  }
}

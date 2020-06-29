package FiveStage
import cats.data.Writer
import cats._
import cats.data._
import cats.implicits._

import fansi._
import PrintUtils._

import fileUtils.say

/**
  * Types and extension methods go here.
  * Maybe it's a litte arbitrary to put VM types here, but op types in Ops.scala
  * Maybe these could be separated to somewhere else.
  */
object Data {
  type Label = String

  case class Reg(value: Int)
  case class Imm(value: Int)
  case class Addr(value: Int){
    def +(that: Addr) = Addr(value + that.value)
    def -(that: Addr) = Addr(value - that.value)
    def step = Addr(value + 4)
  }

  object Reg{ def apply(s: String): Reg = Reg(lookupReg(s).get) }

  type SourceInfo[A] = Writer[List[String], A]
  object SourceInfo { def apply[A](s: String, a: A) = Writer(List(s), a) }

  trait ExecutionEvent
  import PrintUtils._
  case class RegUpdate(reg: Reg, word: Int)  extends ExecutionEvent
  case class MemWrite(addr: Addr, word: Int) extends ExecutionEvent
  case class MemRead(addr: Addr, word: Int)  extends ExecutionEvent

  // addr is the target address
  case class PcUpdateJALR(addr: Addr)                 extends ExecutionEvent
  case class PcUpdateJAL(addr: Addr)                  extends ExecutionEvent
  case class PcUpdateBranch(addr: Addr, target: Addr) extends ExecutionEvent
  case class PcUpdateNoBranch(addr: Addr)             extends ExecutionEvent
  case class PcUpdate(addr: Addr)                     extends ExecutionEvent

  case class ExecutionTraceEvent(pc: Addr, event: ExecutionEvent*){ override def toString(): String = s"$pc: " + event.toList.mkString(", ") }
  type ExecutionTrace[A] = Writer[List[ExecutionTraceEvent], A]

  object ExecutionTrace {
    def apply(vm: VM, event: ExecutionTraceEvent*) = Writer(event.toList, vm)
  }


  sealed trait ChiselEvent
  case class ChiselRegEvent(pcAddr: Addr, reg: Reg, word: Int) extends ChiselEvent
  case class ChiselMemWriteEvent(pcAddr: Addr, memAddr: Addr, word: Int) extends ChiselEvent

  type CircuitTrace = (Addr, List[ChiselEvent])


  /**
    * Not sure these should be defined here instead of in the VM
    */
  case class Regs(repr: Map[Reg, Int]) {
    def +(a: (Reg, Int)): (Option[RegUpdate], Regs) =
      if(a._1.value == 0) (None, this)
      else (Some(RegUpdate(a._1, a._2)), copy(repr + a))

    def arith(rd: Reg, operand1: Reg, operand2: Reg, op: (Int, Int) => Int): (Option[RegUpdate], Regs) =
      this + (rd -> op(repr(operand1), repr(operand2)))

    def arithImm(rd: Reg, operand1: Reg, operand2: Imm, op: (Int, Int) => Int): (Option[RegUpdate], Regs) =
      this + (rd -> op(repr(operand1), operand2.value))

    def compare(operand1: Reg, operand2: Reg, comp: (Int, Int) => Boolean): Boolean =
      comp(repr(operand1), repr(operand2))

    def apply(setting: TestSetting): Regs = setting match {
      case setting: REGSET => Regs(repr + (setting.rd -> setting.word))
      case _ => this
    }
  }


  case class DMem(repr: Map[Addr, Int]) {
    def read(addr: Addr): Either[String, (MemRead, Int)] =
      if(addr.value >= 4096)
        Left(s"attempted to read from illegal address ${addr.show}")
      else {
        val readResult = repr.lift(addr).getOrElse(0)
        Right((MemRead(addr, readResult), readResult))
      }

    def write(addr: Addr, word: Int): Either[String, (MemWrite, DMem)] =
      if(addr.value >= 4096)
        Left(s"attempted to write to illegal address ${addr.show}")
      else {
        Right((MemWrite(addr, word)), DMem(repr + (addr -> word)))
      }

    def apply(setting: TestSetting): DMem = setting match {
      case setting: MEMSET => {
        DMem(repr + (setting.addr -> setting.word))
      }
      case _ => this
    }
  }

  object Regs{
    def empty: Regs = Regs((0 to 31).map(x => (Reg(x) -> 0)).toMap)
    def apply(settings: List[TestSetting]): Regs = settings.foldLeft(empty){
      case(acc, setting) => acc(setting)
    }
  }
  object DMem{
    def empty: DMem = DMem(Map[Addr, Int]())
    def apply(settings: List[TestSetting]): DMem = settings.foldLeft(empty){
      case(acc, setting) => acc(setting)
    }
  }

  


  implicit class IntOps(i: Int) {
    // Needs backticks to not conflict with xml
    def `u>`(that: Int): Boolean = {
      if((i >= 0) && (that >= 0))
        i > that
      else if((i < 0) && (that < 0))
        i > that
      else if((i < 0))
        true
      else
        false
    }

    def nBitsS: Int = i match {
      case i if (i < 0)  => (math.log(math.abs(i.toLong))/math.log(2)).ceil.toInt + 1
      case i if (i == 0) => 0
      case i if (i > 0)  => (math.log((i + 1).toLong)/math.log(2)).ceil.toInt + 1
    }

    /**
      * Yes, a negative number technically has a unsigned size, but that depends on integer width,
      * so it is better left as an option
      */
    def nBitsU: Option[Int] = i match {
      case i if (i < 0)  => None
      case i if (i == 0) => Some(0)
      case i if (i > 0)  => Some((math.log(i)/math.log(2)).ceil.toInt)
    }

    def field(firstBit: Int, size: Int): Int = {
      val bitsLeft = 31 - firstBit
      val bitsRight = 32 - size
      val leftShifted = i << bitsLeft
      val rightShifted = leftShifted >> bitsRight
      rightShifted
    }

    def splitHiLo(hiBits: Int): (Int, Int) = {
      val loBits = 32 - hiBits
      val sep = 31 - hiBits
      val hi = i.field(31, hiBits)
      val lo = i.field(sep, loBits)
      say(s"split lo hi for $i with $hiBits high bits and got low: $lo, high: $hi")
      (hi, lo)
    }

    def log2: Int = math.ceil(math.log(i.toDouble)/math.log(2.0)).toInt

    // Discards two lowest bits
    def getTag(slots: Int): Int = {
      val bitsLeft = 32 - (slots.log2 + 2)
      val bitsRight = 32 - slots.log2
      val leftShifted = i << bitsLeft
      val rightShifted = leftShifted >>> bitsRight
      rightShifted
    }

    // To get the entire word call with from = 31, to = 0
    def bits(from: Int, to: Int): Int = {
      val bitsLeft = 31 - from
      val bitsRight = bitsLeft + to
      val leftShifted = i << bitsLeft
      val rightShifted = leftShifted >>> bitsRight

      rightShifted
    }
  }

  implicit class StringOps(s: String) {
    def binary: Int = {
      s.reverse.foldLeft((0, 0)){
        case((acc, pow), char) if char == '0' => (acc, pow + 1)
        case((acc, pow), char) if char == '1' => (acc + (1 << pow), pow + 1)
        case((acc, pow), char) => assert(false, "malformed binary conversion"); (0, 0)
      }._1
    }
  }

  implicit class ListOps[A](xs: List[A]) {
    def mkStringN = xs.mkString("\n","\n","\n")
    def splitAtPred(p: (A, A) => Boolean): List[List[A]] = {
      val splitPoints = xs.tail.foldLeft((1, List[Int](), xs.head)){
        case((idx, acc, pA), a) if(p(pA, a)) => (1, idx :: acc, a)
        case((idx, acc, pA), a) => (idx + 1, acc, a)
      }._2.reverse

      val (blocks, rem) = splitPoints.foldLeft((List[List[A]](), xs)){
        case((acc, rem), point) => {
          val(block, remainder) = rem.splitAt(point)
          (block :: acc, remainder)
        }
      }
      (rem :: blocks).reverse
    }
    def showN(sep: String)(implicit ev: Fancy[A]): fansi.Str =
      xs.foldLeft(fansi.Str("")){ case(acc, a) => acc ++ a.show ++ fansi.Str(sep) }
    def showN(sep1: String, sep2: String, sep3: String)(implicit ev: Fancy[A]): fansi.Str =
      Str(sep1) ++ xs.foldLeft(fansi.Str("")){ case(acc, a) => acc ++ a.show ++ fansi.Str(sep2) } ++ Str(sep3)
    def shuffle(shuffler: scala.util.Random): List[A] = shuffler.shuffle(xs)
  }
  implicit class NestedListOps[A](xs: List[List[A]]) {
    def zipWithIndexNested: List[List[(A, Int)]] = {
      val startingPoints = xs.scanLeft(0){ case(acc, xs) => acc + xs.size }
      (xs.map(_.zipWithIndex) zip startingPoints).map{ case(withIndex, offset) =>
        withIndex.map{ case(a, idx) => (a, idx + offset) }
      }
    }
  }


  import Ops._

  sealed trait TestSetting
  case class REGSET(rd: Reg, word: Int) extends TestSetting
  case class MEMSET(addr: Addr, word: Int) extends TestSetting

  implicit class ListEitherOps[E,A](es: List[Either[E,A]]) {
    import cats.data.Validated
    def separateXOR: Either[List[E], List[A]] = {
      val (errors, as) = es.map(_.toValidated).separate
      Either.cond(errors.isEmpty, as, errors)
    }
  }


  /**
    * Represents the result of parsing a program, with built in convenience methods for running a VM
    * and assembling the program.
    */
  case class Program(
    ops      : List[SourceInfo[Op]],
    settings : List[TestSetting],
    labelMap : Map[Label, Addr],
  ){

   def imem: Map[Addr, Op] =
     ops.map(_.run._2).zipWithIndex.map{ case(op, idx) => (Addr(idx*4), op) }.toMap


    /**
      * Loads a VM which can be run to get a trace.
      */
   def vm: VM =
     VM(settings, imem, labelMap)


    /**
      * A convenient lookup for every instruction, allowing the test runner to check what source line
      * caused an error to happen.
      */
   val sourceMap: Map[Addr, String] =
     ops.map(_.run._1).zipWithIndex.map{ case(info, idx) => (Addr(idx*4), info.map(_.trim).mkString(", ")) }.toMap


    /**
      * The assembled program
      */
   def machineCode: Either[String, Map[Addr, Int]] =
     imem.toList
       .sortBy(_._1.value).map{ case(addr, op) => assembler.assembleOp(op, addr, labelMap).map(x => (addr, x)) }
       .sequence
       .map(_.toMap)
       .left.map{ case(error, addr) => s"Assembler error: $error, corresponding to source:\n${sourceMap(addr)}" }


    /**
      * Returns the binary code and the execution trace or an error for convenient error checking.
      */
    def validate(maxSteps: Int): Either[String, (Map[Addr, Int], ExecutionTrace[VM])] = machineCode.flatMap{ binary =>
      val uk = "UNKNOWN"
      val (finish, trace) = VM.run(maxSteps, vm)
      finish match {
        case Failed(s, addr) => Left(s"VM failed with error $s at address $addr\nSource line:\n${sourceMap.lift(addr).getOrElse(uk)}")
        case Timeout => Left(s"VM timed out after $maxSteps steps. This should not happen with the supplied tests")
        case Success => Right(binary, trace)
      }
    }

    def labelMapReverse = labelMap.toList.map(_.swap).toMap
  }


  def lookupReg(s: String): Option[Int] = {
    val regMap = Map(
      "x0"       -> 0,
      "x1"       -> 1,
      "x2"       -> 2,
      "x3"       -> 3,
      "x4"       -> 4,
      "x5"       -> 5,
      "x6"       -> 6,
      "x7"       -> 7,
      "x8"       -> 8,
      "x9"       -> 9,
      "x10"      -> 10,
      "x11"      -> 11,
      "x12"      -> 12,
      "x13"      -> 13,
      "x14"      -> 14,
      "x15"      -> 15,
      "x16"      -> 16,
      "x17"      -> 17,
      "x18"      -> 18,
      "x19"      -> 19,
      "x20"      -> 20,
      "x21"      -> 21,
      "x22"      -> 22,
      "x23"      -> 23,
      "x24"      -> 24,
      "x25"      -> 25,
      "x26"      -> 26,
      "x27"      -> 27,
      "x28"      -> 28,
      "x29"      -> 29,
      "x30"      -> 30,
      "x31"      -> 31,
      "zero"     -> 0,
      "ra"       -> 1,
      "sp"       -> 2,
      "gp"       -> 3,
      "tp"       -> 4,
      "t0"       -> 5,
      "t1"       -> 6,
      "t2"       -> 7,
      "s0"       -> 8,
      "fp"       -> 8,
      "s1"       -> 9,
      "a0"       -> 10,
      "a1"       -> 11,
      "a2"       -> 12,
      "a3"       -> 13,
      "a4"       -> 14,
      "a5"       -> 15,
      "a6"       -> 16,
      "a7"       -> 17,
      "s2"       -> 18,
      "s3"       -> 19,
      "s4"       -> 20,
      "s5"       -> 21,
      "s6"       -> 22,
      "s7"       -> 23,
      "s8"       -> 24,
      "s9"       -> 25,
      "s10"      -> 26,
      "s11"      -> 27,
      "t3"       -> 28,
      "t4"       -> 29,
      "t5"       -> 30,
      "t6"       -> 31)

    regMap.lift(s)
  }
}

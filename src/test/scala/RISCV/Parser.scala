package FiveStage
import atto._, Atto._, syntax.refined._
import eu.timepit.refined.numeric._
import fileUtils.say

import Ops._
import Data._

import cats._
import cats.data.{ Op => _ }
import cats.implicits._

object Parser {

  def hex : Parser[Int] = string("0x") ~> many1(hexDigit).map{ ds =>
    val bi = Integer.parseUnsignedInt(new String(ds.toList.toArray), 16)
    bi.toInt
  }

  def labelDest : Parser[Label] = (takeWhile(_ != ':') <~ char(':'))
  def label     : Parser[Label] = takeWhile(_ != ' ')
  def reg       : Parser[Int]   = takeWhile(x => (x != ',' && x != ')')).map(lookupReg).attempt
  def sep       : Parser[Unit]  = many(whitespace) *> char(',') *> many(whitespace).void

  def branch  : (Parser[Int], Parser[Int], Parser[String]) = (reg <~ sep, reg <~ sep, label)
  def branchZ : (Parser[Int], Parser[String]) = (reg <~ sep, label)

  def arith    : (Parser[Int], Parser[Int], Parser[Int]) = (reg <~ sep, reg <~ sep, reg)
  def arithImm : (Parser[Int], Parser[Int], Parser[Int]) = (reg <~ sep, reg <~ sep, (hex | int))

  def stringWs(s: String) : Parser[String] = many(whitespace) ~> string(s) <~ many1(whitespace)

  val singleInstruction: Parser[Op] = List(
    ////////////////////////////////////////////
    //// Branches
    stringWs("beq")   ~> branch.mapN{Branch.beq},
    stringWs("bne")   ~> branch.mapN{Branch.bne},
    stringWs("blt")   ~> branch.mapN{Branch.blt},
    stringWs("bge")   ~> branch.mapN{Branch.bge},
    stringWs("bltu")  ~> branch.mapN{Branch.bltu},
    stringWs("bgeu")  ~> branch.mapN{Branch.bgtu},

    // pseudos:
    stringWs("ble")   ~> branch.mapN{Branch.ble},
    stringWs("bgt")   ~> branch.mapN{Branch.bgt},
    stringWs("bleu")  ~> branch.mapN{Branch.bleu},
    stringWs("bgtu")  ~> branch.mapN{Branch.bgtu},

    // Introduce zero
    stringWs("bnez")  ~> branchZ.mapN{Branch.bnez},
    stringWs("beqz")  ~> branchZ.mapN{Branch.beqz},
    stringWs("blez")  ~> branchZ.mapN{Branch.blez},



    ////////////////////////////////////////////
    //// Arith
    stringWs("add")   ~> arith.mapN{Arith.add},
    stringWs("sub")   ~> arith.mapN{Arith.sub},
    stringWs("or")    ~> arith.mapN{Arith.or},
    stringWs("xor")   ~> arith.mapN{Arith.xor},
    stringWs("and")   ~> arith.mapN{Arith.and},

    stringWs("sll")   ~> arith.mapN{Arith.sll},
    stringWs("srl")   ~> arith.mapN{Arith.srl},
    stringWs("sra")   ~> arith.mapN{Arith.sra},

    stringWs("slt")   ~> arith.mapN{Arith.slt},
    stringWs("sltu")  ~> arith.mapN{Arith.sltu},

    // pseudos
    stringWs("mv")    ~> (reg <~ sep, reg, ok(0)).mapN{Arith.add},
    stringWs("nop")   ~> (ok(0), ok(0), ok(0)).mapN{Arith.add},

    // Check if rs1 is not equal to 0. 
    // snez rd, rs1 => sltu rd, zero, rs1
    stringWs("snez")  ~> (reg <~ sep, ok(0), reg).mapN{Arith.sltu},


    ////////////////////////////////////////////
    //// Arith Imm
    stringWs("addi")   ~> arithImm.mapN{ArithImm.add},
    stringWs("ori")    ~> arithImm.mapN{ArithImm.or},
    stringWs("xori")   ~> arithImm.mapN{ArithImm.xor},
    stringWs("andi")   ~> arithImm.mapN{ArithImm.and},
    
    stringWs("slli")   ~> arithImm.mapN{ArithImmShift.sll},
    stringWs("srli")   ~> arithImm.mapN{ArithImmShift.srl},
    stringWs("srai")   ~> arithImm.mapN{ArithImmShift.sra},
   
    stringWs("slti")   ~> arithImm.mapN{ArithImm.slt},
    stringWs("sltiu")  ~> arithImm.mapN{ArithImm.sltu},

    // pseudos
    stringWs("not")   ~> (reg <~ sep, reg, ok(-1)).mapN{ArithImm.xor},

    // Check if rs1 is less than 1. Only 0 is less than 1 when using unsigned comparison
    // seqz rd, rs1 => sltiu rd, rs1, 1
    stringWs("seqz")  ~> (reg <~ sep, reg, ok(1)).mapN{ArithImm.sltu},

    stringWs("li")    ~> (reg ~ sep ~ (hex | int)).collect{
      case((a, b), c) if (c.nBitsS <= 12) => {
        say(s"for c: $c, nBitsS was ${c.nBitsS}")
        ArithImm.add(a, 0, c)
      }
    },


    ////////////////////////////////////////////
    //// Jumps
    stringWs("jalr")  ~> (reg <~ sep, reg <~ sep, label).mapN{JALR.apply},
    stringWs("jal")   ~> (reg <~ sep, label).mapN{JAL.apply},

    // pseudos
    // JAL with ra as rd automatically chosen.
    stringWs("call")  ~> label.map(label => JAL(regNames.ra, label)),

    // For jr we don't care about where we jumped from.
    stringWs("jr")    ~> reg.map(r => JALR(0, r, "zero")),

    // As jr, but with a label rather than a register.
    stringWs("j")     ~> label.map(label => JAL(0, label)),
    many(whitespace)  ~> string("ret") ~> ok(JALR(0, regNames.ra, "zero")),


    ////////////////////////////////////////////
    //// load/store
    stringWs("sw") ~> (reg <~ sep, (hex | int) <~ char('('), reg <~ char(')')).mapN{case (rs2, offset, rs1) => Store.sw(rs2, rs1, offset)},
    stringWs("sh") ~> (reg <~ sep, (hex | int) <~ char('('), reg <~ char(')')).mapN{case (rs2, offset, rs1) => Store.sh(rs2, rs1, offset)},
    stringWs("sb") ~> (reg <~ sep, (hex | int) <~ char('('), reg <~ char(')')).mapN{case (rs2, offset, rs1) => Store.sb(rs2, rs1, offset)},

    stringWs("lw")  ~> (reg <~ sep, (hex | int) <~ char('('), reg <~ char(')')).mapN{case (rd, offset, rs1) => Load.lw(rd, rs1, offset)},
    stringWs("lh")  ~> (reg <~ sep, (hex | int) <~ char('('), reg <~ char(')')).mapN{case (rd, offset, rs1) => Load.lh(rd, rs1, offset)},
    stringWs("lb")  ~> (reg <~ sep, (hex | int) <~ char('('), reg <~ char(')')).mapN{case (rd, offset, rs1) => Load.lb(rd, rs1, offset)},
    stringWs("lhu") ~> (reg <~ sep, (hex | int) <~ char('('), reg <~ char(')')).mapN{case (rd, offset, rs1) => Load.lhu(rd, rs1, offset)},
    stringWs("lbu") ~> (reg <~ sep, (hex | int) <~ char('('), reg <~ char(')')).mapN{case (rd, offset, rs1) => Load.lbu(rd, rs1, offset)},



    
    ////////////////////////////////////////////
    //// others
    stringWs("auipc") ~> (reg <~ sep, (hex | int)).mapN{AUIPC.apply},
    stringWs("lui")   ~> (reg <~ sep, (hex | int)).mapN{LUI.apply},

    many(whitespace)  ~> string("nop") ~> ok(Arith.add(0, 0, 0)),
    many(whitespace)  ~> string("done") ~> ok(DONE),
  ).map(_.widen[Op]).reduce(_|_)


  val multipleInstructions: Parser[List[Op]] = List(
    stringWs("li") ~> (reg <~ sep, (hex | int).map(_.splitHiLo(20))).mapN{ case(rd, (hi, lo)) => {
      say("hello?")
      List(
      ArithImm.add(rd, rd, lo),
      LUI(rd, hi),
    )}}.map(_.widen[Op]),
  ).reduce(_|_)


  val instruction = singleInstruction.map(List(_)) | multipleInstructions

  val setting = List(
    char('#') ~> string("regset") ~> many1(whitespace) ~> (reg.map(Reg.apply)  <~ sep, hex | int).mapN{REGSET.apply},
    char('#') ~> string("memset") ~> many1(whitespace) ~> ((hex | int).map(Addr.apply) <~ sep, hex | int).mapN{MEMSET.apply}
  ).map(_.widen[TestSetting]).reduce(_|_)


  def parseProgram(p: List[String], testOptions: TestOptions): Either[String, Program] = {

    val all = setting || (instruction || labelDest)

    /**
      * The foldhelper represents a traversal through a RISC-V program. 
      * 
      * When it sees an op it records the operation and appends the source line and its location.
      * If it is in nopPad mode it will also insert NOPs between the parsed ops.
      * After appending ops the address counter is bumbed accordingly
      * 
      * When it sees a label destination it checks what the current addres counter is at and creates
      * a link to this address.
      * 
      * When it sees a parse error it simply stores the error and keeps going, allowing you to get every error
      * (This works for an ASM program since each line is independent)
      * 
      * Lastly, when it sees a test setting it appends that test setting.
      * 
      * The reason everything is treated all-in-one is to make it easier to ensure that everything is parsed.
      * If there were separate parsers for ops, labels and settings it would be difficult to find out if errors
      * were simply of the wrong type or a legit error.
      * This is not set in stone, if you're re-architecturing the code maybe it's better to separate parsers?
      * Or maybe have multiple passes? Up to you!
      */
    case class FoldHelper(
      settings  : List[TestSetting],
      ops       : List[SourceInfo[Op]],
      labelMap  : Map[Label, Addr],
      errors    : List[String],
      addrCount : Int){
      def addSettings (t: TestSetting): FoldHelper = copy(settings = t :: settings)
      def addErrors   (t: String): FoldHelper = copy(errors = t :: errors)
      def addLabelMap (t: Label): FoldHelper = copy(labelMap = labelMap + (t -> Addr(addrCount)))
      def addOps      (t: List[SourceInfo[Op]]): FoldHelper = {
        if(testOptions.nopPadded){
          copy(
            ops = t.flatMap(x => (x :: List.fill(4)(SourceInfo("inserted NOP", NOP).widen[Op]))).reverse ::: ops,
            addrCount = addrCount + t.size*4*5)
        }
        else {
          copy(ops = t ::: ops, addrCount = addrCount + t.size*4)
        }
      }
      def program: Either[String, (List[TestSetting], List[SourceInfo[Op]], Map[Label, Addr])] = {
      /**
        * There are two possible ways for a program to successfully terminate, either by explicitly executing
        * a DONE instruction, or by returning from the main method.
        * In the latter case it is necessary to preload the return address register such that the return instruction
        * jumps to a predetermined special done address 
        */
        val hasDONEinstruction = ops.map(_.run._2).contains(DONE)
        val done = copy(settings = REGSET(Reg("sp"), 1024) :: settings, ops = ops.reverse, errors = errors.reverse)

        val withReturnAddress = if(hasDONEinstruction){
          done
        }
        else
          done.copy(settings = REGSET(Reg("ra"), 0xEB1CEB1C) :: done.settings) // now that's what I call EPIC

        Either.cond(errors.isEmpty, (withReturnAddress.settings, done.ops, labelMap), done.errors).left.map(errors =>
          s"Parser errors in ${testOptions.testName}:\n" + errors.mkString("\n"))
      }
    }


    def foldHelper(
      acc: FoldHelper,
      program: List[(Int, String)]): FoldHelper = program match {
      case Nil => acc
      case (lineNo, line) :: t => {
        if(line.isEmpty)
          foldHelper(acc, t)
        else {
          val next = all.parse(line).done.either match {
            case Left(parseError)           => acc.addErrors(f"$lineNo%3d" +s":$line\t$parseError")
            case Right(Left(setting))       => acc.addSettings(setting)
            case Right(Right(Right(label))) => acc.addLabelMap(label)
            case Right(Right(Left(ops)))    => acc.addOps(ops.map(op => SourceInfo(s"${lineNo.toString.padTo(3, ' ')}:\t$line", op)))
          }
          foldHelper(next, t)
        }
      }
    }

    val results = foldHelper(FoldHelper(Nil, Nil, Map("zero" -> Addr(0)), Nil, 0), p.zipWithIndex.map(_.swap))

    results.program.map{ case(settings, ops, labelMap) => Program(ops, settings, labelMap) }
  }


  def lookupReg(s: String): Int = {
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

    regMap(s)
  }
}

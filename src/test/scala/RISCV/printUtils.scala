package FiveStage
import cats.data.Writer
import cats._
import cats.data.{ Op => _ }
import cats.implicits._

import fansi._

import Ops._
import Data._
import VM._
import fileUtils._

object PrintUtils {

  // Minimal typeclass def for being fancy
  trait Fancy[-A] { def s(a: A): fansi.Str }
  implicit class FancyOps[-A](a: A)(implicit ev: Fancy[A]){ def show: fansi.Str = ev.s(a) }
  implicit val DoubleFancy = new Fancy[fansi.Str]{ def s(a: fansi.Str) = a }
  implicit val FancyMonoid = new Monoid[fansi.Str] {
    def empty: fansi.Str = fansi.Str("")
    def combine(self: fansi.Str, that: fansi.Str): fansi.Str = self ++ that
  }

  implicit class ExtraFancy(a: fansi.Str) {
    def leftPad(length: Int): fansi.Str = Str((0 until length).map(_ => ' ').mkString) ++ a
    def replace(from: Char, to: Char): fansi.Str = fansi.Str(a.toString.replace(from, to))
    def padTo(length: Int, pad: Char = ' '): fansi.Str = a ++ (0 until length - a.length).map(_ => pad).mkString
    def trim: fansi.Str = fansi.Str(a.toString.trim)
  }

  implicit val RegFancy  = new Fancy[Reg] { def s(r: Reg) = fansi.Str(regNames.canonicalName.lift(r.value).getOrElse("ERROR")) }
  implicit val ImmFancy  = new Fancy[Imm] { def s(r: Imm) = Str(r.value.hs) }
  implicit val AddrFancy = new Fancy[Addr]{ def s(r: Addr) = Str(r.value.hs) }

  implicit val ExecutionEventFancy = new Fancy[ExecutionEvent]{
    def s(a: ExecutionEvent): fansi.Str = a match {
      case RegUpdate(reg, word) => Str(s"R(${reg.show}) <- 0x${word.hs}")
      case MemWrite(addr, word) => fansi.Color.Blue(s"M[${addr.show}] <- 0x${word.hs}")
      case MemRead(addr, word)  => fansi.Color.Red(f"M[${addr.show}] -> 0x${word.hs}")

      // addr is the target address
      case PcUpdateJALR(addr)       => fansi.Color.Green(s"PC updated to ${addr.show} via JALR")
      case PcUpdateJAL(addr)        => fansi.Color.Magenta(s"PC updated to ${addr.show} via JAL")
      case PcUpdateBranch(from, to) => fansi.Color.Yellow(s"PC updated to ${to.show} via Branch")
      case PcUpdateNoBranch(addr)   => fansi.Color.Yellow(s"PC updated to ${addr.show}, skipping a Branch")
    }
  }

  implicit val ExecutionTraceEventFancy = new Fancy[ExecutionTraceEvent]{
    def s(a: ExecutionTraceEvent): fansi.Str = a.event.toList match {
      case (h: ExecutionEvent) :: (t: RegUpdate) :: Nil => t.show.padTo(25) ++ h.show
      case (h: MemWrite) :: t :: Nil => t.show.padTo(25) ++ h.show
      case (h: MemRead)  :: t :: Nil => t.show.padTo(25) ++ h.show
      case (h: MemWrite) :: Nil      => h.show.leftPad(25)
      case (h: MemRead)  :: Nil      => h.show.leftPad(25)
      case h :: t :: Nil => h.show ++ t.show
      case (h: RegUpdate) :: Nil => h.show
      case h :: Nil => h.show.leftPad(25)
      case _ => Str("")
    }
  }

  implicit val ChiselEventFancy = new Fancy[ChiselEvent]{
    def s(e: ChiselEvent) = e match {
      case ChiselRegEvent(addr, reg, word) => fansi.Str(s"R(${reg.show}) <- ${word.hs}")
      case ChiselMemWriteEvent(addr, memAddr, word) => fansi.Str(s"M[${memAddr.show}] <- ${word.hs}")
    }
  }

  implicit val RegsFancy = new Fancy[Regs]{
    def s(e: Regs) = {
      (0 to 9).map{ idx =>
        val cols = List.range(idx, 32, 10)
        cols.map{ reg =>
          (fansi.Color.Yellow(Reg(reg).show.padTo(5)) ++ Str(e.repr.lift(Reg(reg)).getOrElse(0).hs)).padTo(16)
        }.showN("| ")
      }.toList.showN("\n", "\n", "\n")
    }
  }

  val UNKNOWN = "UNKNOWN"

  def printInstruction(op: Ops.Op, labelMap: Map[Label, Addr]): fansi.Str = op match {
    case op: Branch        => fansi.Color.Red(s"B${op.comp}\t${op.rs1.show}, ${op.rs2.show}, ${op.dst.show}\t[${labelMap.lift(op.dst).getOrElse(UNKNOWN)}]")
    case op: Arith         => s"${op.op}\t${op.rd.show}, ${op.rs1.show}, ${op.rs2.show}"
    case op: ArithImm      => s"${op.op}I\t${op.rd.show}, ${op.rs1.show}, ${op.imm.show}"
    case op: ArithImmShift => s"${op.op}I\t${op.rd.show}, ${op.rs1.show}, ${op.shamt.show}"
    case op: JALR          => fansi.Color.Green(s"JALR\t${op.rd.show}, ${op.rs1.show}, ${op.dst.show}\t[${labelMap.lift(op.dst).getOrElse(UNKNOWN)}]")
    case op: JAL           => fansi.Color.Magenta(s"JAL\t${op.rd.show}, ${op.dst.show} [${labelMap.lift(op.dst).getOrElse(UNKNOWN)}]")
    case op: LW            => s"LW\t${op.rd.show}, ${op.offset.show}(${op.rs1.show})"
    case op: SW            => s"SW\t${op.rs2.show}, ${op.offset.show}(${op.rs1.show})"
    case op: LUI           => s"LUI\t${op.rd.show}, ${op.imm.show}"
    case op: AUIPC         => s"AUIPC\t${op.rd.show}, ${op.imm.show}"
    case DONE              => s"DONE"
  }


  implicit class IntPrinters(i: Int) {
    def hs: String = if(i > 65535) f"0x$i%08X" else f"0x$i%04X"
    def binary: String = String.format("%" + 32 + "s", i.toBinaryString)
      .replace(' ', '0').grouped(4)
      .map(x => x + "  ").mkString
    def binary(n: Int): String = String.format("%" + n + "s", i.toBinaryString).replace(' ', '0')
  }



  def printProgram(vm: VM): String = {
    val withLabels: List[Either[(Addr, Op), (Label, Addr)]] = (vm.imem.toList.map(Left(_)) ::: vm.labelMap.toList.map(Right(_)))
      .sortBy { case Left(x) => x._1.value.toDouble + 0.1; case Right(x) => x._2.value.toDouble }

    withLabels.map{
        case Left((addr, op)) => s"$addr:\t\t${printInstruction(op, vm.labelMap)}"
        case Right((label, addr)) => s"$addr:\t$label:"
      }.mkString("\n","\n","\n")
  }


  def printProgram(p: Program): String = printProgram(p.vm)


  def printBinary(bin: Map[Addr, Int]): String = {
    bin.toList.sortBy(_._1.value).map{ case(addr, op) => s"$addr: ${op.hs}\t--\t${op.binary}" }.mkString("\n","\n","\n")
  }


  def printChiselLogEntry(event: (Addr, List[ChiselEvent])): fansi.Str = {
    val (addr, log) = event
    val events = log match {
      case (h: ChiselRegEvent)      :: (t: ChiselMemWriteEvent) :: Nil => h.show.padTo(25, ' ') ++ t.show
      case (h: ChiselMemWriteEvent) :: Nil                             => h.show.leftPad(25)
      case (h: ChiselRegEvent)      :: Nil                             => h.show
      case _ => fansi.Str("")
    }
    events
  }


  def printVMtrace(trace: List[ExecutionTraceEvent], program: Program): String = {
    val blocks: List[List[(ExecutionTraceEvent, Int)]] = LogParser.splitToBlocks(trace).zipWithIndexNested
    blocks.map{ block =>
      val name = LogParser.guessBlockName(block.map(_._1.pc), program.labelMapReverse) 
      val blockString = block.map{ case(event, idx) =>
        Str(s"Step: $idx,").padTo(12) ++
        Str("PC: ") ++
        event.pc.show ++
        Str("\t") ++
        event.show.padTo(70) ++
        printSourceLine(event.pc, program)
      }
      fansi.Color.Yellow(name) ++ Str("\n") ++ blockString.showN("\n")
    }.mkStringN
  }

  def printSourceLine(addr: Addr, program: Program): fansi.Str = program.sourceMap.lift(addr).map(fansi.Str(_)).getOrElse(fansi.Str("???"))

  def printMergedTraces(
    mt: (List[ExecutionTraceEvent], List[CircuitTrace]),
    program: Program
  ): List[String] = {


    /**
      * For branch predicting processors we may end up with a spurious block causing a transient
      * desynchronization. Therefore it is necessary to have a certain tolerance before calling a divergence.
      */
    // TODO: Implement a synchronization test to pinpoint where a desync happens
    // This is not straight forward in the case of processors which perform branch prediction.
    def isSynchronized(desyncs: Int, traces: (List[ExecutionTraceEvent], List[CircuitTrace]) ): Boolean = ???


    def helper(mt: (List[ExecutionTraceEvent], List[CircuitTrace])): List[List[fansi.Str]] = mt match {
        /**
          * VM trace and execution log synchronized, step both
          */
        case (hVM :: tVM, hC :: tC) if (hVM.pc == hC._1) => {
          val address        = hVM.pc
          val chiselLogEntry = printChiselLogEntry(hC)
          val vmEntry        = hVM.show
          val sourceLine     = printSourceLine(address, program)

          val line = List(address.show, chiselLogEntry, vmEntry, sourceLine)

          line :: helper((tVM, tC))
        }


        /**
          * VM trace and execution log unsynchronized, step only chisel trace
          * Helper is called with (hVM :: tVM) to only step the chisel log.
          */
        case (hVM :: tVM, hC :: tC) => {
          val address        = hC._1
          val chiselLogEntry = printChiselLogEntry(hC)
          val vmEntry        = Str("")
          val sourceLine     = printSourceLine(address, program)

          val line = List(address.show, chiselLogEntry, vmEntry, sourceLine)

          line :: helper((hVM :: tVM, tC))
        }


        /**
          * The Block contains no more instructions performed by the VM. This
          * happens naturally since 5-stage pipelines will fetch spurious instructions.
          */
        case (Nil, hC :: tC) => {
          val address        = hC._1
          val chiselLogEntry = printChiselLogEntry(hC)
          val vmEntry        = fansi.Str("")
          val sourceLine     = printSourceLine(address, program)

          val line = List(fansi.Color.LightBlue(address.show), chiselLogEntry, vmEntry, sourceLine)

          line :: helper((Nil, tC))
        }


        /**
          * The Block contains no more instructions performed by the circuit.
          * This happens when the circuit takes a jump the VM does not.
          * This is an error *unless* the circuit has a branch predictor.
          * 
          * If you want to you can splice the logs when this happens, but it's typically easy to spot.
          */
        case (hVM :: tVM, Nil) => {
          val address        = hVM.pc
          val chiselLogEntry = fansi.Str("")
          val vmEntry        = hVM.show
          val sourceLine     = printSourceLine(address, program)

          val line = List(fansi.Color.LightRed(address.show), chiselLogEntry, vmEntry, sourceLine)

          line :: helper((tVM, Nil))
        }

        case (Nil, Nil) => Nil
      }


    def format(triplet: List[fansi.Str]): String = {
      // Ahh, the GNU toolchain and its tabs
      val annoyingTabCharacter = '	'
      triplet match {
        case address :: ch :: vm :: source :: Nil => {
          val vmLine: fansi.Str = vm.padTo(60, ' ')
          val chiselLine: fansi.Str = ch.padTo(50, ' ')
          val sourceLines: fansi.Str = source
          ((address ++ Str(":")).padTo(14, ' ') ++ vmLine ++ fansi.Str("|    ") ++ chiselLine ++ Str("|    ") ++ sourceLines).toString
        }
        case _ => ""
      }
    }

    val blockName = LogParser.guessBlockName(mt._1.map(_.pc), program.labelMapReverse)

    (fansi.Color.Yellow(s"$blockName").padTo(74, ' ').toString ++ "|".padTo(55, ' ') ++ "|") :: helper(mt).map(format)
  }


  def printChiselError(e: Throwable): String = {
    val rawError = """
      |This typically occurs when you forget to wrap a module
      |e.g
      |
      |class MyBundle extends Bundle {
      |  val signal = UInt(32.W)
      |}
      |val mySignal = new MyBundle
      |               ^^^^ Wrong!
      |should be
      |val mySignal = Wire(new MyBundle)
      """.stripMargin

    def getFiveStageStacktrace(strace: Array[StackTraceElement]): String =
      strace.map(_.toString).filter(_.contains("FiveStage")).toList.take(5).mkStringN

    e match {
      case e: firrtl.passes.CheckInitialization.RefNotInitializedException => {
        s"""
        |##########################################################
        |##########################################################
        |##########################################################
        |Your design has unconnected wires!"
        |error:\n"
        |${e.getMessage}
        |
        |
        |##########################################################
        |##########################################################
        |##########################################################
        """.stripMargin
      }
      case e: chisel3.core.Binding.ExpectedHardwareException => {
        s"""
        |##########################################################
        |##########################################################
        |##########################################################
        |Your design is using raw chisel types!
        |error:\n
        |${e.getMessage}
        |
        |$rawError
        |The error should be here somewhere:
        |${getFiveStageStacktrace(e.getStackTrace)}
        |##########################################################
        |##########################################################
        |##########################################################
        """.stripMargin
      }
      case e: firrtl.passes.PassExceptions => {
        val rawErrorMsg = if(e.getMessage.contains("raw"))
          s"One of the errors was use of raw chisel types. $rawError"
        else
          ""

        s"""
        |##########################################################
        |##########################################################
        |##########################################################
        |Your design has multiple errors!
        |error:
        |${e.getMessage}
        |$rawErrorMsg
        |##########################################################
        |##########################################################
        |##########################################################
        """.stripMargin
      }
      case e: Exception => {
        s"""
        |##########################################################
        |##########################################################
        |##########################################################
        |Unexpected Chisel tester error (could be a chisel error I havent accounted for, or a bug in the tester like index out of bounds.)
        |error:
        |${e.getMessage}
        |reduced stacktrace:
        |${getFiveStageStacktrace(e.getStackTrace)}
        |##########################################################
        |##########################################################
        |##########################################################
        """.stripMargin
      }
    }
  }

  def printLogSideBySide(trace: List[ExecutionTraceEvent], chiselTrace: List[CircuitTrace], program: Program): String = {
    import LogParser._
    val header = "ADDRESS   --   VM UPDATES                                                ---      DEVICE UNDER TEST UPDATES                     ---    CORRESPONDING SOURCE LINE\n"
    val traces = mergeTraces(trace, chiselTrace).map(x => printMergedTraces((x), program))
    "\n" + header + (traces.map(_.mkString("\n")).mkString("\n", "\n--------------------------------------------------------------------------+------------------------------------------------------+-------------------------------\n", "\n"))
  }
}

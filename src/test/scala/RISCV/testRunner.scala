package FiveStage
import org.scalatest.{Matchers, FlatSpec}
import cats._
import cats.implicits._
import fileUtils._

import chisel3.iotesters._
import scala.collection.mutable.LinkedHashMap

import fansi.Str

import Ops._
import Data._
import VM._

import PrintUtils._
import LogParser._

case class TestOptions(
  printIfSuccessful  : Boolean,
  printErrors        : Boolean,
  printParsedProgram : Boolean,
  printVMtrace       : Boolean,
  printVMfinal       : Boolean,
  printMergedTrace   : Boolean,
  printBinary        : Boolean,
  nopPadded          : Boolean,
  breakPoints        : List[Int], // Not implemented
  testName           : String,
  maxSteps           : Int
)

case class TestResult(
  regError   : Option[String],
  memError   : Option[String],
  program    : String,
  vmTrace    : String,
  vmFinal    : String,
  sideBySide : String,
  binary     : String
)

object TestRunner {

  def run(testOptions: TestOptions): Boolean = {

    val testResults = for {
      lines                           <- fileUtils.readTest(testOptions)
      program                         <- FiveStage.Parser.parseProgram(lines, testOptions)
      (binary, (trace, finalVM))      <- program.validate(testOptions.maxSteps).map(x => (x._1, x._2.run))
      (termitationCause, chiselTrace) <- ChiselTestRunner(
        binary.toList.sortBy(_._1.value).map(_._2),
        program.settings,
        finalVM.pc,
        testOptions.maxSteps,
        testOptions.testName)
    } yield {
      val traces = mergeTraces(trace, chiselTrace).map(x => printMergedTraces((x), program))

      val programString = printProgram(program)
      val vmTraceString = printVMtrace(trace, program)
      val vmFinalState = finalVM.regs.show
      val traceString = printLogSideBySide(trace, chiselTrace, program)
      val binaryString = printBinary(binary)

      val regError = compareRegs(trace, chiselTrace)
      val memError = compareMem(trace, chiselTrace)

      TestResult(
        regError,
        memError,
        programString,
        vmTraceString,
        vmFinalState.toString,
        traceString,
        binaryString)
    }

    testResults.left.foreach{ error =>
      say(s"Test was unable to run due to error: $error")
    }

    testResults.map{ testResults =>
      val successful = List(testResults.regError, testResults.memError).flatten.headOption.map(_ => false).getOrElse(true)
      if(successful)
        sayGreen(s"${testOptions.testName} succesful")
      else
        sayRed(s"${testOptions.testName} failed")

      if(testOptions.printIfSuccessful && successful){
        if(testOptions.printParsedProgram) say(testResults.program)
        if(testOptions.printVMtrace)       say(testResults.vmTrace)
        if(testOptions.printVMfinal)       say(testResults.vmFinal)
        if(testOptions.printMergedTrace)   say(testResults.sideBySide)
        if(testOptions.printBinary)        say(testResults.binary)
      }
      else{
        if(testOptions.printErrors){
          say(testResults.regError.map(_.show.toString).getOrElse("no reg errors"))
          say(testResults.memError.map(_.show.toString).getOrElse("no mem errors"))
        }
        if(testOptions.printParsedProgram) say(testResults.program)
        if(testOptions.printVMtrace)       say(testResults.vmTrace)
        if(testOptions.printVMfinal)       say(testResults.vmFinal)
        if(testOptions.printMergedTrace)   say(testResults.sideBySide)
        if(testOptions.printBinary)        say(testResults.binary)
      }
      successful
    }.toOption.getOrElse(false)
  }

  def profileBranching(testOptions: TestOptions): Boolean = {

    val testResults = for {
      lines                           <- fileUtils.readTest(testOptions)
      program                         <- FiveStage.Parser.parseProgram(lines, testOptions)
      (binary, (trace, finalVM))      <- program.validate(testOptions.maxSteps).map(x => (x._1, x._2.run))
    } yield {

      sealed trait BranchEvent
      case class Taken(from: Int, to: Int) extends BranchEvent { override def toString = s"Taken      ${from.hs}\t${to.hs}" }
      case class NotTaken(addr: Int) extends BranchEvent { override def toString =       s"Not Taken  ${addr.hs}" }

      val events: List[BranchEvent] = trace.flatMap(_.event).collect{
        case PcUpdateBranch(from, to) => Taken(from.value, to.value)
        case PcUpdateNoBranch(at) => NotTaken(at.value)
      }


      /**
        * This is a sample profiler for a rather unrealistic branch predictor which has an unlimited amount
        * of slots
        */
      def OneBitInfiniteSlots(events: List[BranchEvent]): Int = {

        // Uncomment to take a look at the event log
        // say(events.mkString("\n","\n","\n"))

        // Helper inspects the next element of the event list. If the event is a mispredict the prediction table is updated
        // to reflect this.
        // As long as there are remaining events the helper calls itself recursively on the remainder
        def helper(events: List[BranchEvent], predictionTable: Map[Int, Boolean]): Int = {
          events match {

            // Scala syntax for matching a list with a head element of some type and a tail
	          // `case h :: t =>`
	          // means we want to match a list with at least a head and a tail (tail can be Nil, so we
	          // essentially want to match a list with at least one element)
	          // h is the first element of the list, t is the remainder (which can be Nil, aka empty)

	          // `case Constructor(arg1, arg2) :: t => `
	          // means we want to match a list whose first element is of type Constructor, giving us access to its internal
	          // values.

	          // `case Constructor(arg1, arg2) :: t => if(p(arg1, arg2))`
	          // means we want to match a list whose first element is of type Constructor while satisfying some predicate p,
	          // called an if guard.
            case Taken(from, to) :: t if( predictionTable(from)) => helper(t, predictionTable)
            case Taken(from, to) :: t if(!predictionTable(from)) => 1 + helper(t, predictionTable.updated(from, true))
            case NotTaken(addr)  :: t if( predictionTable(addr)) => 1 + helper(t, predictionTable.updated(addr, false))
            case NotTaken(addr)  :: t if(!predictionTable(addr)) => helper(t, predictionTable)
            case Nil => 0
          }
        }

        // Initially every possible branch is set to false since the initial state of the predictor is to assume branch not taken
        def initState = events.map{
          case Taken(from, addr) => (from, false)
          case NotTaken(addr)    => (addr, false)
        }.toMap

        helper(events, initState)
      }

      say(OneBitInfiniteSlots(events))
    }


    true
  }


  def profileCache(testOptions: TestOptions): Boolean = {

    val testResults = for {
      lines                           <- fileUtils.readTest(testOptions)
      program                         <- FiveStage.Parser.parseProgram(lines, testOptions)
      (binary, (trace, finalVM))      <- program.validate(testOptions.maxSteps).map(x => (x._1, x._2.run))
    } yield {

      sealed trait MemoryEvent
      case class Write(addr: Int) extends MemoryEvent
      case class Read(addr: Int) extends MemoryEvent

      val events: List[MemoryEvent] = trace.flatMap(_.event).collect{
        case MemWrite(x,_) => Write(x.value)
        case MemRead(x,_) => Read(x.value)
      }

      // Your cache here

    }
    true
  }
}

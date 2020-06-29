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

object BranchProfiler {

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


      def twoBitPredictor(events: List[BranchEvent], slots: Int): Int = {

        case class nBitPredictor(
          values          : List[Int],
          predictionRules : List[Boolean],
          transitionRules : Int => Boolean => Int,
        ){
          val slots = values.size

          def predict(pc: Int): Boolean = predictionRules(values(pc.getTag(slots)))

          def update(pc: Int, taken: Boolean): nBitPredictor = {
            val current = values(pc.getTag(slots))
            val next = copy(values = values.updated(pc.getTag(slots), transitionRules(current)(taken)))
            next
          }

          override def toString = values.map(x => x.binary(2)).mkString("[","][","]")
        }

        val initPredictor = nBitPredictor(
          List.fill(slots)(0),
          List(
            false,
            false,
            true,
            true,
          ),
          r => r match {
            case 0 => taken => if(taken) 1 else 0
            case 1 => taken => if(taken) 2 else 0
            case 2 => taken => if(taken) 3 else 1
            case 3 => taken => if(taken) 3 else 2
          }
        )

        events.foldLeft((0, initPredictor)){ case(((acc, bp), event)) =>
          println()
          say(s"total misses: $acc")
          say(event)
          event match {
            case Taken(pc, _) => say(s"taken at tag:     ${pc.getTag(slots)}")
            case NotTaken(pc) => say(s"not taken at tag: ${pc.getTag(slots)}")
          }
          say(bp)
          event match {
            case Taken(pc, _) if bp.predict(pc)  => {say("HIT!");  (acc,     bp.update(pc, true))}
            case Taken(pc, _)                    => {say("MISS!"); (acc + 1, bp.update(pc, true))}
            case NotTaken(pc) if !bp.predict(pc) => {say("HIT!");  (acc,     bp.update(pc, false))}
            case NotTaken(pc)                    => {say("MISS!"); (acc + 1, bp.update(pc, false))}
          }
        }._1
      }

      say(events.mkString("\n","\n","\n"))
      say(twoBitPredictor(events, 8))
    }


    true
  }
}

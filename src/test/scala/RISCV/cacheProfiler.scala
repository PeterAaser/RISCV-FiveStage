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

object CacheProfiler {
  
  def profileCache(testOptions: TestOptions): Boolean = {

    val testResults = for {
      lines                           <- fileUtils.readTest(testOptions)
      program                         <- FiveStage.Parser.parseProgram(lines, testOptions)
      (binary, (trace, finalVM))      <- program.validate(testOptions.maxSteps).map(x => (x._1, x._2.run))
    } yield {

      import TestUtils._

      sealed trait MemoryEvent
      case class Write(addr: Int) extends MemoryEvent
      case class Read(addr: Int) extends MemoryEvent

      val events = trace.flatMap(_.event).collect{
        case MemWrite(addr, _) => Write(addr.value)
        case MemRead(addr, _) => Read(addr.value)
      }


      class CacheProfiler(setCount: Int, setSize: Int, blockSize: Int){

        // If we set counter to 0 we risk evicting the first allocated block.
        var counter = 1
        var misses = 0
        var mostRecent = 0
        var wasMiss = false

        implicit class AddrOps(i: Int){
          val blockOffsetBits = blockSize.log2
          val lineBits = setSize.log2

          def lineIdx: Int = {
            i.bits(2 + blockOffsetBits + lineBits - 1, 2 + blockOffsetBits)
          }
        }


        case class CacheLine(tag: Int, lastUsed: Int){
          def matches(addr: Int): Boolean = List.fill(blockSize)(tag)
            .zipWithIndex
            .map{ case(btag, idx) => btag + idx*4 }
            .map(_ == addr)
            .foldLeft(false)(_ || _)

          def renderContent(addr: Int): String = (addr == mostRecent, wasMiss) match {
            case (true, true)  => Console.RED + addr.hs + Console.RESET
            case (true, false) => Console.GREEN + addr.hs + Console.RESET
            case _ => addr.hs
          }

          def render: String = {
            val blockContents = List.fill(blockSize)(tag)
              .zipWithIndex
              .map{ case(btag, idx) => renderContent(btag + idx*4) }
              .mkString("Contents: || ", " | ", " |")

            s"Base: ${tag.hs} LRU: $lastUsed\t" + blockContents
          }
        }
        object CacheLine {
          def truncateTag(addr: Int) = addr - (addr % (blockSize*4))
        }


        case class CacheSet(blocks: Array[CacheLine]){
          def lineIdx(addr: Int): Int = addr.lineIdx
          def contains(addr: Int): Boolean = blocks.map(_.matches(addr)).foldLeft(false)(_ || _)

          def updateLRU(addr: Int): Unit = {
            val idx = lineIdx(addr)
            val next = blocks(idx).copy(lastUsed = counter)
            blocks(idx) = next
          }

          def render: String = {
            blocks.map(_.render).mkString("\n", "\n", "\n")
          }
        }


        case class Cache(sets: Array[CacheSet]){

          /** returns the index of set if hit */
          def checkHit(addr: Int): Option[Int] = sets
            .zipWithIndex
            .map{ case(set, idx) => Option.when(set.contains(addr))(idx) }
            .flatten.headOption


          /** Updates the LRU counter */
          def updateLRU(addr: Int, setIdx: Int): Unit = sets(setIdx).updateLRU(addr)


          /** Gets set with least recently used */
          def getLRU(addr: Int): Int = sets
            .map( set => set.blocks(set.lineIdx(addr)).lastUsed)
            .zipWithIndex
            .sortBy(_._1)
            .map(_._2)
            .head


          /** Entry point */
          def handleAccess(addr: Int): Unit = {
            mostRecent = addr
            counter += 1

            checkHit(addr) match {

              case Some(setIdx) => {
                wasMiss = false
                updateLRU(addr, setIdx)
                // say(s"${addr.hs} HIT")
              }

              case None => {
                val set = sets(getLRU(addr))
                val nextTag = CacheLine.truncateTag(addr)
                set.blocks(set.lineIdx(addr)) = set.blocks(set.lineIdx(addr)).copy(
                  tag = nextTag,
                  lastUsed = counter
                )
                misses += 1

                wasMiss = true
                // say(s"${addr.hs} MISS")
                // say(s"BLOCK ${addr.lineIdx} IN SET ${getLRU(addr)} EVICTED. BYE BYE")
              }
            }
          }

          /** Pretty pictures! */
          def render: String = {
            sets.map(_.render).mkString("\n", "\n", "\n")
          }
        }

        object Cache {
          def init: Cache = Cache(Array.fill(setCount)(
            CacheSet(Array.fill(setSize)(CacheLine(57005, 0))))
          )
        }
      }

      for{
        sets <- List(2, 4, 8)
        blockSize <- List(4, 8)
        lines <- List(2, 4, 8)
      } yield {

        val myTest = new CacheProfiler(sets, lines, blockSize)
        val myCache = myTest.Cache.init
        events.foreach{
          case Write(addr) => myCache.handleAccess(addr)
          case Read(addr) => myCache.handleAccess(addr)
        }

        say(s"sets: $sets, lines: $lines, blockSize: $blockSize yields ${myTest.misses} misses")
      }

      // val myTest = new CacheProfiler(2, 4, 4)
      // val myCache = myTest.Cache.init
      // events.foreach{
      //   case Write(addr) => {
      //     say(addr.hs)
      //     myCache.handleAccess(addr)
      //     say(myCache.render)
      //   }
      //   case Read(addr) => {
      //     say(addr.hs)
      //     myCache.handleAccess(addr)
      //     say(myCache.render)
      //   }
      // }

      // say(myTest.misses)

    }

    true
  }
}

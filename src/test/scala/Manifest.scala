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

object Manifest {

  val singleTest = "forward2.s"

  val nopPadded = true

  val singleTestOptions = TestOptions(
    printIfSuccessful  = true,
    printErrors        = true,
    printParsedProgram = false,
    printVMtrace       = false,
    printVMfinal       = false,
    printMergedTrace   = true,
    nopPadded          = nopPadded,
    breakPoints        = Nil, // not implemented
    testName           = singleTest)


  val allTestOptions: String => TestOptions = name => TestOptions(
    printIfSuccessful  = false,
    printErrors        = false,
    printParsedProgram = false,
    printVMtrace       = false,
    printVMfinal       = false,
    printMergedTrace   = false,
    nopPadded          = nopPadded,
    breakPoints        = Nil, // not implemented
    testName           = name)

}


class SingleTest extends FlatSpec with Matchers {
  it should "just werk" in {
    TestRunner.run(Manifest.singleTestOptions) should be(true)
  }
}


class AllTests extends FlatSpec with Matchers {
  it should "just werk" in {
    val werks = getAllTestNames.map{testname =>
      say(s"testing $testname")
      val opts = Manifest.allTestOptions(testname)
      (testname, TestRunner.run(opts))
    }
    if(werks.foldLeft(true)(_ && _._2))
      say(Console.GREEN + "All tests successful!" + Console.RESET)
    else {
      val success = werks.map(x => if(x._2) 1 else 0).sum
      val total   = werks.size
      say(s"$success/$total tests successful")
      werks.foreach{ case(name, success) =>
        val msg = if(success) Console.GREEN + s"$name successful" + Console.RESET
        else Console.RED + s"$name failed" + Console.RESET
        say(msg)
      }
    }
  }
}

class WindowsAllTests extends FlatSpec with Matchers {
  it should "just werk" in {
    val werks = getAllWindowsTestNames.map{testname =>
      say(s"testing $testname")
      val opts = Manifest.allTestOptions(testname)
      (testname, TestRunner.run(opts))
    }
    if(werks.foldLeft(true)(_ && _._2))
      say(Console.GREEN + "All tests successful!" + Console.RESET)
    else {
      val success = werks.map(x => if(x._2) 1 else 0).sum
      val total   = werks.size
      say(s"$success/$total tests successful")
      werks.foreach{ case(name, success) =>
        val msg = if(success) Console.GREEN + s"$name successful" + Console.RESET
        else Console.RED + s"$name failed" + Console.RESET
        say(msg)
      }
    }
  }
}
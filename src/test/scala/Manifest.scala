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
    testName           = singleTest,
    maxSteps           = 15000)


  val allTestOptions: String => TestOptions = name => TestOptions(
    printIfSuccessful  = false,
    printErrors        = false,
    printParsedProgram = false,
    printVMtrace       = false,
    printVMfinal       = false,
    printMergedTrace   = false,
    nopPadded          = nopPadded,
    breakPoints        = Nil, // not implemented
    testName           = name,
    maxSteps           = 15000)

}



class ProfileBranching extends FlatSpec with Matchers {
  it should "profile some branches" in {
    TestRunner.profileBranching(
      Manifest.singleTestOptions.copy(testName = "branchProfiling.s", maxSteps = 50000)
    ) should be(true)
  }
}

class ProfileCache extends FlatSpec with Matchers {
  it should "profile a cache" in {
    say("Warning, this test takes forever to run! 2 minutes on my machine at least.")
    say("This happens due to the less than optimal way of storing the update log. Sorry I guess")
    say("You probably want to debug this with a smaller program")
    TestRunner.profileCache(
      Manifest.singleTestOptions.copy(testName = "convolution.s", maxSteps = 150000)
    ) should be(true)
  }
}

class SingleTest extends FlatSpec with Matchers {
  it should "just werk" in {
    TestRunner.run(Manifest.singleTestOptions) should be(true)
  }
}


class AllTests extends FlatSpec with Matchers {
  it should "just werk" in {
    val werks = getAllTestNames.filterNot(_ == "convolution.s").map{testname => 
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

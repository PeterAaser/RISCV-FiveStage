package FiveStage
import chisel3.iotesters._
import scala.collection.mutable.LinkedHashMap

import fileUtils.say

import Data._
import PrintUtils._

private class ChiselTestRunner (
  instructions    : List[Int],
  settings        : List[TestSetting],
  c               : Tile,
  d               : PeekPokeTester[Tile],
  terminalAddress : Addr,
  maxSteps        : Int) {


  /**
    * Currently unused as it is quite nontrivial to figure out whether read data is actually used.
    * Still, with the necessary scaffolding these can be correlated with reg writes to figure out
    * if they are valid or not.
    */
  case class ChiselMemReadEvent(pcAddr: Addr, memAddr: Addr, word: Int)

  private def setup = {

    d.poke(d.dut.io.setup, 1)

    val initDMem = DMem(settings).repr.toList
    val initRegs = Regs(settings).repr.toList

    setupImem(instructions)
    setupRegs(initRegs)
    setupDmem(initDMem)

    // flush
    d.step(1)
    disableTestSignals
    d.step(5)
    d.poke(d.dut.io.setup, 0)

    def disableTestSignals: Unit = {
      d.poke(d.dut.io.DMEMWriteData, 0)
      d.poke(d.dut.io.DMEMAddress, 0)
      d.poke(d.dut.io.DMEMWriteEnable, 0)
      d.poke(d.dut.io.regsWriteData, 0)
      d.poke(d.dut.io.regsAddress, 0)
      d.poke(d.dut.io.regsWriteEnable, 0)
      d.poke(d.dut.io.IMEMWriteData, 0)
      d.poke(d.dut.io.IMEMAddress, 4092)
    }

    def setupRegs(regs: List[(Reg, Int)]) = {
      regs.foreach{ case(reg, word) =>
        d.poke(d.dut.io.setup, 1)
        d.poke(d.dut.io.regsWriteEnable, 1)
        d.poke(d.dut.io.regsWriteData, BigInt(word))
        d.poke(d.dut.io.regsAddress, reg.value)
        d.step(1)
      }
      d.poke(d.dut.io.regsWriteEnable, 0)
      d.poke(d.dut.io.regsWriteData, 0)
      d.poke(d.dut.io.regsAddress, 0)
    }

    def setupDmem(mem: List[(Addr, Int)]) = {
      mem.foreach { case (addr, word) =>
        d.poke(d.dut.io.setup, 1)
        d.poke(d.dut.io.DMEMWriteEnable, 1)
        d.poke(d.dut.io.DMEMWriteData, word)
        d.poke(d.dut.io.DMEMAddress, addr.value)
        d.step(1)
      }
      d.poke(d.dut.io.DMEMWriteEnable, 0)
      d.poke(d.dut.io.DMEMWriteData, 0)
      d.poke(d.dut.io.DMEMAddress, 0)

    }

    def setupImem(instructions: List[Int]) = {
      (0 until instructions.length).foreach{ ii =>
        d.poke(d.dut.io.IMEMAddress, ii*4)
        d.poke(d.dut.io.IMEMWriteData, instructions(ii).toInt)
        d.step(1)
      }
      d.poke(d.dut.io.IMEMAddress, 4092) // Ensures that we don't overwrite an instruction. Bandaid for lack of IMEM.writeEnable
      d.poke(d.dut.io.IMEMWriteData, 0)
    }
  }


  private def getPC               = Addr(d.peek(d.dut.io.currentPC).toInt)
  private def getDMemWriteAddress = Addr(d.peek(d.dut.io.memDeviceWriteAddress).toInt)
  private def getRegWriteAddress  = Reg(d.peek(d.dut.io.regsDeviceWriteAddress).toInt)

  private def getRegUpdate: Option[ChiselRegEvent] = {
    if(
      (d.peek(d.dut.io.regsDeviceWriteEnable)  == 1) &&
      (getRegWriteAddress.value != 0)
    ){
      val regWriteAddress = getRegWriteAddress
      val regWriteData    = d.peek(d.dut.io.regsDeviceWriteData).toInt
      val regUpdate = ChiselRegEvent(getPC, regWriteAddress, regWriteData)
      Some(regUpdate)
    }
    else
      None
  }

  private def getMemWrite: Option[ChiselMemWriteEvent] = {
    if(d.peek(d.dut.io.memDeviceWriteEnable) == 1) {
      val memWriteAddress = getDMemWriteAddress
      val memWriteData    = d.peek(d.dut.io.memDeviceWriteData).toInt
      val memUpdate = ChiselMemWriteEvent(getPC, memWriteAddress, memWriteData)
      Some(memUpdate)
    }
    else
      None
  }

  private def stepOne: CircuitTrace = {
    val state = (getPC, List(getRegUpdate, getMemWrite).flatten)
    // say(d.peek(d.dut.io).toList.mkStringN)
    d.step(1)
    state
  }


  private def go(log: List[CircuitTrace], timeOut: Int): (Option[String], List[CircuitTrace]) = {
    if(timeOut == 0){
      (Some("Chisel tester timed out before reaching termination address"), log.reverse)
    }
    else if(getPC == terminalAddress){
      (None, (flush ::: log).reverse)
    }
    else {
      val step = stepOne
      go(step :: log, timeOut - 1)
    }
  }

  // After finishing, let the circuit run until all updates can be committed.
  private def flush: List[CircuitTrace] =
    (0 to 4).map(_ => stepOne).reverse.toList

  /**
    * Run the entire shebang
    */
  def run: (Option[String], List[CircuitTrace]) = {
    setup
    go(Nil, maxSteps)
  }
}

object ChiselTestRunner {

  def apply(
    binary          : List[Int],
    settings        : List[TestSetting],
    terminalAddress : Addr,
    maxSteps        : Int,
    testName        : String): Either[String, (Option[String], List[CircuitTrace])] = {

    var sideEffectExtravaganza: Option[(Option[String], List[CircuitTrace])] = None

    val error: Either[String, Boolean] = scala.util.Try {
      chisel3.iotesters.Driver.execute(Array(
                                         "--generate-vcd-output", "on",
                                         "--backend-name", "treadle",
                                         "--target-dir", "waveforms",
                                         "--top-name", testName
                                       ), () => new Tile) { c =>
        new PeekPokeTester(c) {
          val testRunner = new ChiselTestRunner(
            binary,
            settings,
            c,
            this,
            terminalAddress,
            maxSteps
          )

          val log = testRunner.run
          sideEffectExtravaganza = Some(log)
        }
      }
    }.toEither.left.map{printChiselError}

    error.flatMap{ e =>
      sideEffectExtravaganza.toRight("Unknown test failure, please let me know")
    }
  }
}

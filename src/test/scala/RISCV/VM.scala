package FiveStage
import cats._
import cats.data.{ Op => _ }
import cats.implicits._
import fileUtils._

import Data._
import Ops._

import PrintUtils._

sealed trait Finished
case object Success extends Finished
case object Timeout extends Finished
case class Failed(s: String, addr: Addr) extends Finished

case class VM(
  dmem     : DMem,
  imem     : Map[Addr, Op],
  regs     : Regs,
  pc       : Addr,
  labelMap : Map[Label, Addr]){
  def stepInstruction: Either[Finished, ExecutionTrace[VM]] = {
    if (pc.value == 0xEB1CEB1C) Left(Success)
    else getOp flatMap {
      case op: Branch        => executeBranch(op)
      case op: Arith         => executeArith(op)
      case op: ArithImm      => executeArithImm(op)
      case op: ArithImmShift => executeArithImmShift(op)
      case op: AUIPC         => executeAUIPC(op)
      case op: LUI           => executeLUI(op)
      case op: JALR          => executeJALR(op)
      case op: JAL           => executeJAL(op)
      case op: LW            => executeLW(op)
      case op: SW            => executeSW(op)
      case DONE              => Left(Success)
    }
  }


  private def executeBranch(op: Branch) = {
    getAddr(op.dst).map{ addr =>
      val takeBranch = regs.compare(op.rs1, op.rs2, op.comp.run)
      if(takeBranch){
        val nextVM = copy(pc = addr)
        jump(nextVM, PcUpdateBranch(pc, nextVM.pc))
      }
      else {
        step(this, PcUpdateNoBranch(this.pc + Addr(4)))
      }
    }
  }

  /**
    * The weird :_* syntax is simply a way to pass a list to a varArgs function.
    * 
    * To see why, consider def printMany(a: Any*); printMany(List(1,2,3))
    * It is now ambiguous if it should print "1, 2, 3" or List(1, 2, 3)
    * 
    * This is disambiguated by appending :_*, thus
    * printMany(List(1,2,3):_*) == printMany(1, 2, 3)
    */
  private def executeArith(op: Arith) = {
    val (regUpdate, nextRegs) = regs.arith(op.rd, op.rs1, op.rs2, op.op.run)
    val nextVM = this.copy(regs = nextRegs)
    Right(step(nextVM, regUpdate.toList:_*))
  }


  private def executeArithImm(op: ArithImm) = {
    val (regUpdate, nextRegs) = regs.arithImm(op.rd, op.rs1, op.imm, op.op.run)
    val nextVM = this.copy(regs = nextRegs)
    Right(step(nextVM, regUpdate.toList:_*))
  }


  private def executeArithImmShift(op: ArithImmShift) = {
    val (regUpdate, nextRegs) = regs.arithImm(op.rd, op.rs1, op.shamt, op.op.run)
    val nextVM = this.copy(regs = nextRegs)
    Right(step(nextVM, regUpdate.toList:_*))
  }


  private def executeLUI(op: LUI) = {
    val (regUpdate, nextRegs) = regs + (op.rd -> (op.imm.value << 12))
    val nextVM = this.copy(regs = nextRegs)
    Right(step(nextVM, regUpdate.toList:_*))
  }


  private def executeAUIPC(op: AUIPC) = {
    val (regUpdate, nextRegs) = regs + (op.rd -> (pc.value << 12))
    val nextVM = this.copy(regs = nextRegs)
    Right(step(nextVM, regUpdate.toList:_*))
  }



  private def executeJALR(op: JALR) = getAddr(op.dst).map{ targetAddr =>
    val nextPc = Addr((regs.repr(op.rs1).value + targetAddr.value) & 0xFFFFFFFE)
    val (regUpdate, nextRegs) = regs + (op.rd -> (pc.step.value))
    val nextVM = this.copy(regs = nextRegs, pc = nextPc)
    jump(nextVM, (PcUpdateJALR(nextPc) :: regUpdate.toList):_*)
  }


  private def executeJAL(op: JAL) = getAddr(op.dst).map{ targetAddr =>
    val nextPc = targetAddr
    val (regUpdate, nextRegs) = regs + (op.rd -> (pc.step.value))
    val nextVM = this.copy(regs = nextRegs, pc = nextPc)
    jump(nextVM, (PcUpdateJAL(nextPc) :: regUpdate.toList):_*)
  }


  private def executeLW(op: LW) = dmem.read(Addr(regs.repr(op.rs1) + op.offset.value)).map{ case(event, result) =>
    val (regUpdate, nextRegs) = regs + (op.rd -> result)
    val nextVM = this.copy(regs = nextRegs)
    step(nextVM, (event :: regUpdate.toList):_*)
  }.left.map(x => Failed(x, pc))

  private def executeSW(op: SW) = {
    val writeAddress = Addr(regs.repr(op.rs1) + op.offset.value)
    val writeData = regs.repr(op.rs2)
    dmem.write(writeAddress, writeData).map{ case(event, nextDmem) =>
      val nextVM = this.copy(dmem = nextDmem)
      step(nextVM, event)
    }
  }.left.map(x => Failed(x, pc))

  private def step(nextVM: VM, event: ExecutionEvent*) =
    ExecutionTrace(nextVM.stepPC, ExecutionTraceEvent(pc, event:_*))

  // Same as above, but no stepping
  private def jump(nextVM: VM, event: ExecutionEvent*) =
    ExecutionTrace(nextVM, ExecutionTraceEvent(pc, event:_*))


  private def stepPC: VM = copy(pc = this.pc.step)

  private def getAddr(dst: Label): Either[Failed, Addr] =
    labelMap.lift(dst).toRight(Failed(s"Label $dst missing", pc))

  private def getOp: Either[Failed, Op] =
    imem.lift(pc).toRight(Failed(s"Attempted to fetch instruction at illegal address ${pc.show}", pc))
}


object VM {

  val init = VM(DMem.empty, Map[Addr, Op](), Regs.empty, Addr(0), Map[Label, Addr]())

  def apply(settings: List[TestSetting], imem: Map[Addr, Op], labelMap: Map[Label, Addr]): VM = {
    val (dmem, regs) = settings.foldLeft((DMem.empty, Regs.empty)){ case((dmem, regs), setting) => setting match {
        case setting: REGSET => (dmem, regs(setting))
        case setting: MEMSET => (dmem(setting), regs)
      }
    }
    VM(dmem, imem, regs, Addr(0), labelMap)
  }

  def run(maxSteps: Int, vm: VM) = {
    def helper(state: ExecutionTrace[VM], step: Int): (Finished, ExecutionTrace[VM]) = {
      if(step > 0){
        val (log, vm) = state.run
        val next = vm.stepInstruction
        next match {
          case Left(stopped) => (stopped, state)
          case Right(trace) => helper((state >> trace), step - 1)
        }
      }
      else{
        (Timeout, state)
      }
    }
    helper(ExecutionTrace(vm), maxSteps)
  }
}

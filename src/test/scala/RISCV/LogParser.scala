package FiveStage
import Data._
import fileUtils.say
import PrintUtils._

/**
  * Helpers for comparing VM and chisel logs
  */
object LogParser {


  /**
    * Peeks ahead at the chisel log to see if an expected jump is taken.
    */
  def fetchUntilJumpTarget(addr: Addr, chiselLog: List[CircuitTrace]): Option[List[CircuitTrace]] = {
    val (head, tail) = chiselLog.splitAt(4) // very arbitrary choice
    val pruned: List[CircuitTrace] = head.dropWhile{ case(myAddr, _) => myAddr != addr }
    pruned.headOption.map(_ => pruned ::: tail)
  }



  /**
    * Fetches a basic block of VM execution trace
    */
  def splitToBlocks(vmTrace: List[ExecutionTraceEvent]): List[List[ExecutionTraceEvent]] =
    vmTrace.splitAtPred{ case(current, next) =>
      !(next.pc == current.pc + Addr(4))
    }


  /**
    * Fetches a basic block of chisel execution trace
    */
  def splitToBlocksChisel(chiselTrace: List[CircuitTrace]): List[List[CircuitTrace]] =
    chiselTrace.splitAtPred{ case((current, _), (next, _)) =>
      !((next == current + Addr(4)) || (next == current))
    }


  def guessBlockName(trace: List[Addr], labelMap: Map[Addr, Label]): String = trace.headOption.map(x => labelMap.lift(x)
    .getOrElse(labelMap.lift(trace.head - Addr(4)).getOrElse("UNKNOWN (return jump or misjump)"))).getOrElse("UNKNOWN")


  /**
    * Attempts to merge blocks, patching up when mismatches occur
    * Fails when branchpredictor misjumps, feel free to send a PR
    */
  type BlockList = List[(List[ExecutionTraceEvent], List[CircuitTrace])]

  def mergeTraces(vmTrace: List[ExecutionTraceEvent], chiselTrace: List[CircuitTrace]): BlockList = {
    def helper(acc: BlockList, blocs: (List[List[ExecutionTraceEvent]], List[List[CircuitTrace]])): BlockList = blocs match {

      case (vmBlock :: vmTail, chiselBlock :: chiselTail) if (vmBlock.head.pc == chiselBlock.head._1) =>
        helper((vmBlock, chiselBlock) :: acc, ((vmTail, chiselTail)))

      case (vmBlock :: vmTail, chiselBlock :: chiselTail) =>
        helper((Nil, chiselBlock) :: acc, ((vmBlock :: vmTail, chiselTail)))

      case (Nil, chiselBlock :: chiselTail) =>
        helper((Nil, chiselBlock) :: acc, ((Nil, chiselTail)))

      case (vmBlock :: vmTail, Nil) =>
        helper((vmBlock, Nil) :: acc, ((vmTail, Nil)))

      case _ => acc.reverse
    }
    helper(Nil, (splitToBlocks(vmTrace), splitToBlocksChisel(chiselTrace)))
  }


  /**
    * Compares register update logs
    */
  def compareRegs(vmTrace: List[ExecutionTraceEvent], chiselTrace: List[CircuitTrace]): Option[String] = {
    val vmRegUpdates = vmTrace.zipWithIndex
      .flatMap{ case (e, step) => e.event.toList.map(y => (step, e.pc, y))}
      .collect{ case (step: Int, addr: Addr, x: RegUpdate) => (step, addr, x) }

    val chiselRegUpdates = chiselTrace.zipWithIndex
      .flatMap{ case (e, step) => e._2.map(y => (step, e._1, y))}
      .collect{ case (step: Int, addr: Addr, x: ChiselRegEvent) => (step, addr, x) }

    val errors = (vmRegUpdates zip chiselRegUpdates).map{
      case((_, _, vmUpdate), (_, _, chiselUpdate)) if ((vmUpdate.reg == chiselUpdate.reg) && (vmUpdate.word == chiselUpdate.word)) => {
        None
      }

      case((vmStep, vmAddr, vmUpdate), (chiselStep, chiselAddr, chiselUpdate)) => {
        val errorString = s"Register update mismatch.\n" ++
        s"VM:      At step $vmStep, at address ${vmAddr.show}, the VM got ${vmUpdate.show}\n" ++
        s"Circuit: At step $chiselStep, at address ${chiselAddr.show}, the circuit got ${chiselUpdate.show}"
        Some(errorString)
      }
    }

    val error = errors.collect{ case Some(x) => x }.headOption


    val lengthMismatch: Option[String] = (vmRegUpdates, chiselRegUpdates) match {
      case (h :: t, Nil) => Some(s"Your design performed no reg updates. First expected update was at VM step ${h._1}, PC: ${h._2.show}, ${h._3.show}")
      case (Nil, h :: t) => Some(s"Your design performed reg updates, but the VM did not. First update was at step ${h._1}, PC: ${h._2.show}, ${h._3.show}")
      case (hVM :: tVM, hC :: tC) if (tVM.size > tC.size) => {

        val VMremainder = tVM.drop(tC.size)

        val errorString =
          s"VM performed more reg updates than your design.\n" ++
          s"Your design performed ${chiselRegUpdates.size} updates before terminating, while the VM performed ${vmRegUpdates.size} updates.\n" ++
          s"The first update your design missed happened at VM step ${VMremainder.head._1}, PC: ${VMremainder.head._2.show} and was ${VMremainder.head._3.show}"

        Some(errorString)
      }
      case (hVM :: tVM, hC :: tC) if (tVM.size < tC.size) => {
        val ChiselRemainder = tC.drop(tVM.size)

        val errorString =
          s"Your design performed more reg updates than the VM.\n" ++
          s"Your design performed ${chiselRegUpdates.size} updates before terminating, while the VM performed ${vmRegUpdates.size} updates.\n" ++
          s"The first spurious update your design did happened at cycle ${ChiselRemainder.head._1}, PC: ${ChiselRemainder.head._2.show} and was ${ChiselRemainder.head._3.show}"

        Some(errorString)
      }

      case _ => None
    }


    (error :: lengthMismatch :: Nil).flatten.headOption
  }


  def compareMem(vmTrace: List[ExecutionTraceEvent], chiselTrace: List[CircuitTrace]): Option[String] = {
    val vmMemUpdates = vmTrace.zipWithIndex
      .flatMap{ case (e, step) => e.event.toList.map(y => (step, e.pc, y))}
      .collect{ case (step: Int, addr: Addr, x: MemWrite) => (step, addr, x) }

    val chiselMemUpdates = chiselTrace.zipWithIndex
      .flatMap{ case (e, step) => e._2.map(y => (step, e._1, y))}
      .collect{ case (step: Int, addr: Addr, x: ChiselMemWriteEvent) => (step, addr, x) }

    val error = (vmMemUpdates zip chiselMemUpdates).map{
      case((_, _, vmUpdate), (_, _, chiselUpdate)) if ((vmUpdate.addr == chiselUpdate.memAddr) && (vmUpdate.word == chiselUpdate.word)) =>
        None
      case((vmStep, vmAddr, vmUpdate), (chiselStep, chiselAddr, chiselUpdate)) => {
        val errorString = s"Mem update mismatch.\n" ++
        s"VM:      At step $vmStep, at address ${vmAddr.show}, the VM got ${vmUpdate.show}\n" ++
        s"Circuit: At step $chiselStep, at address ${chiselAddr.show}, the circuit got ${chiselUpdate.show}"
        Some(errorString)
      }
    }.collect{ case Some(x) => x }.headOption


    val lengthMismatch = (vmMemUpdates, chiselMemUpdates) match {
      case (h :: t, Nil) => Some(s"Your design performed no mem updates. First expected update was at VM step ${h._1}, PC: ${h._2}, ${h._3}")
      case (Nil, h :: t) => Some(s"Your design performed mem updates, but the VM did not. First spurious update was at step ${h._1}, PC: ${h._2}, ${h._3}")
      case (hVM :: tVM, hC :: tC) if (tVM.size > tC.size) => {

        val VMremainder = tVM.drop(tC.size)

        val errorString =
          s"VM performed more mem updates than your design.\n" ++
          s"Your design performed ${chiselMemUpdates.size} updates before terminating, while the VM performed ${vmMemUpdates.size} updates.\n" ++
          s"The first update your design missed happened at VM step ${VMremainder.head._1}, PC: ${VMremainder.head._2} and was ${VMremainder.head._3}"

        Some(errorString)
      }
      case (hVM :: tVM, hC :: tC) if (tVM.size < tC.size) => {
        val ChiselRemainder = tC.drop(tVM.size)

        val errorString =
          s"Your design performed more mem updates than the VM.\n" ++
          s"Your design performed ${chiselMemUpdates.size} updates before terminating, while the VM performed ${vmMemUpdates.size} updates.\n" ++
          s"The first spurious update your design did happened at cycle ${ChiselRemainder.head._1}, PC: ${ChiselRemainder.head._2} and was ${ChiselRemainder.head._3}"

        Some(errorString)
      }

      case _ => None
    }

    (error :: lengthMismatch :: Nil).flatten.headOption
  }
}

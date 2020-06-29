package FiveStage

import fileUtils._
import Data._
import PrintUtils._

object TestUtils {

  implicit class OptionBackport(t: Option.type){
    def when[T](b: Boolean)(t: => T) = if(b) Some(t) else None
  }

  /**
    * Generate and serialize BTrees for the test runner
    */
  def generateBTree: Unit = {

    import cats._
    import cats.implicits._

    case class AnnotatedNode(value: Int, index: Int, left: Option[AnnotatedNode], right: Option[AnnotatedNode]){
      def find(key: Int): Unit = {
        say(s"at $index (${(4 + (index << 2)).hs})-> ${value.hs}, looking for ${key.hs}")
        if(value == key)
          say("found it")
        else if(key < value)
          left.map{x => say("going left\n"); x.find(key)}.getOrElse("gave up")
        else
          right.map{x => say("going right\n"); x.find(key)}.getOrElse("gave up")
      }
    }

    def printAnnoTree(tree: AnnotatedNode, depth: Int): String = {
      val ls = tree.left.map(n => printAnnoTree(n, depth + 2)).getOrElse("left empty")
      val rs = tree.right.map(n => printAnnoTree(n, depth + 2)).getOrElse("right empty")
      val pads = "|".padTo(2, ' ')*depth
      s"${tree.value.hs} at ${tree.index.hs}\n$pads$ls\n$pads$rs"
    }

    case class Node(value: Int, left: Option[Node], right: Option[Node]){
      def append(v: Int): Node = if(v < value)
        copy(left = left.map(_.append(v)).orElse(Some(Node(v))))
      else
        copy(right = right.map(_.append(v)).orElse(Some(Node(v))))
    }

    object Node {
      def apply(v: Int): Node = Node(v, None, None)
    }

    def annotate(n: Int, root: Node): (AnnotatedNode, Int) = {
      (root.left, root.right) match {
        case (None, None) => {
          (AnnotatedNode(root.value, n, None, None), n + 1)
        }
        case (Some(node), None) => {
          val (annotated, next) = annotate(n+1, node)
          (AnnotatedNode(root.value, n, Some(annotated), None), next)
        }
        case (None, Some(node)) => {
          val (annotated, next) = annotate(n+1, node)
          (AnnotatedNode(root.value, n, None, Some(annotated)), next)
        }
        case (Some(left), Some(right)) => {
          val (leftAnno, leftNext) = annotate(n+1, left)
          val (rightAnno, rightNext) = annotate(leftNext, right)
          (AnnotatedNode(root.value, n, Some(leftAnno), Some(rightAnno)), rightNext)
        }
      }
    }

    def foldAnno(root: Option[AnnotatedNode]): List[Int] = {
      root.map{ root =>
        val leftIndex = root.left.map(_.index).getOrElse(0)
        val hasLeft = root.left.map(_ => 1).getOrElse(0)

        val rightIndex = root.right.map(_.index).getOrElse(0)
        val hasRight = root.right.map(_ => 1).getOrElse(0)

        val entry = hasLeft + (leftIndex << 1) + (hasRight << 8) + (rightIndex << 9) + (root.value  << 16)

        // say(s"with leftIndex: ${leftIndex.hs}, rightIndex: ${rightIndex.hs}, value: ${root.value.hs} we got ${entry.hs}")

        entry :: foldAnno(root.left) ::: foldAnno(root.right)
      }.getOrElse(Nil)
    }

    import scala.util.Random
    val r = new scala.util.Random(0xF01D1EF7)
    def randInt = r.nextInt(1000)

    val seed = (0 to 100).map(_ => randInt).toList
    val btree = seed.foldLeft(Node(randInt, None, None)){ case(acc, n) => acc.append(n)}

    val annoTree = annotate(0, btree)
    say(foldAnno(Some(annoTree._1)).zipWithIndex.map{case(m, idx) => s"#memset ${(4 + (idx*4)).hs}, ${m.hs}"}.mkStringN)
  }


  /**
    * Generates a random program filled with hazards
   */
  def generateHazardsForward(steps: Int) : Unit = {

    val r = new scala.util.Random(0xF01D1EF7)
    // val r = new scala.util.Random(0xF01D1EF8)
    import Ops._

    val active = List(1, 2, 3)

    val initVM = {
      val init = VM.init
      val init1 = init.copy(regs = (init.regs + (Reg(1) -> 123))._2)
      val init2 = init.copy(regs = (init1.regs + (Reg(2) -> -40))._2)
      val init3 = init.copy(regs = (init2.regs + (Reg(3) -> 0xFFEE))._2)
      init3
    }
    

    def generateInstruction: (Int, (String, Op)) = {
      val rd  = active.shuffle(r).head
      val rs1 = active.shuffle(r).head
      val rs2 = active.shuffle(r).head
      val imm = r.nextInt(1024) - 512
      val shift = r.nextInt(32) - 16

      val choices = List(
        (s"add ${Reg(rd).show}, ${Reg(rs1).show}, ${Reg(rs2).show}", Arith.add(rd, rs1, rs2)),
        (s"sub ${Reg(rd).show}, ${Reg(rs1).show}, ${Reg(rs2).show}", Arith.sub(rd, rs1, rs2)),
        (s"or ${Reg(rd).show}, ${Reg(rs1).show}, ${Reg(rs2).show}", Arith.or(rd, rs1, rs2)),
        (s"xor ${Reg(rd).show}, ${Reg(rs1).show}, ${Reg(rs2).show}", Arith.xor(rd, rs1, rs2)),
        (s"and ${Reg(rd).show}, ${Reg(rs1).show}, ${Reg(rs2).show}", Arith.and(rd, rs1, rs2)),
        (s"sll ${Reg(rd).show}, ${Reg(rs1).show}, ${Reg(rs2).show}", Arith.sll(rd, rs1, rs2)),
        (s"srl ${Reg(rd).show}, ${Reg(rs1).show}, ${Reg(rs2).show}", Arith.srl(rd, rs1, rs2)),
        (s"sra ${Reg(rd).show}, ${Reg(rs1).show}, ${Reg(rs2).show}", Arith.sra(rd, rs1, rs2)),
        (s"slt ${Reg(rd).show}, ${Reg(rs1).show}, ${Reg(rs2).show}", Arith.slt(rd, rs1, rs2)),
        (s"sltu ${Reg(rd).show}, ${Reg(rs1).show}, ${Reg(rs2).show}", Arith.sltu(rd, rs1, rs2)),

        (s"addi ${Reg(rd).show}, ${Reg(rs1).show}, ${Imm(imm).show}", ArithImm.add(rd, rs1, imm)),
        (s"ori ${Reg(rd).show}, ${Reg(rs1).show}, ${Imm(imm).show}", ArithImm.or(rd, rs1,  imm)),
        (s"xori ${Reg(rd).show}, ${Reg(rs1).show}, ${Imm(imm).show}", ArithImm.xor(rd, rs1, imm)),
        (s"andi ${Reg(rd).show}, ${Reg(rs1).show}, ${Imm(imm).show}", ArithImm.and(rd, rs1, imm)),
        (s"slli ${Reg(rd).show}, ${Reg(rs1).show}, ${Imm(math.abs(shift).toInt % 32).show}", ArithImmShift.sll(rd, rs1, math.abs(shift).toInt % 32)),
        (s"srli ${Reg(rd).show}, ${Reg(rs1).show}, ${Imm(math.abs(shift).toInt % 32).show}", ArithImmShift.srl(rd, rs1, math.abs(shift).toInt % 32)),
        (s"srai ${Reg(rd).show}, ${Reg(rs1).show}, ${Imm(math.abs(shift).toInt % 32).show}", ArithImmShift.sra(rd, rs1, math.abs(shift).toInt % 32)),
        (s"slti ${Reg(rd).show}, ${Reg(rs1).show}, ${Imm(imm).show}", ArithImm.slt(rd, rs1, imm)),
        (s"sltiu ${Reg(rd).show}, ${Reg(rs1).show}, ${Imm(imm).show}", ArithImm.sltu(rd, rs1, imm)))
      (rd, choices.shuffle(r).head)
    }

    def helper(attempts: Int, steps: Int, vm: VM): Unit = {
      if((attempts > 10) || (steps == 0))
        ()
      else{
        val (nextRd, (nS, nextOp)) = generateInstruction
        val withOp = vm.copy(imem = vm.imem + (vm.pc -> nextOp))
        val nextVmE = withOp.stepInstruction
        val nextVm = nextVmE.toOption.get.run._2
        if(nextVm.regs.repr(Reg(nextRd)) == 0)
          helper(attempts + 1, steps, vm)
        else {
          say(nS)
          helper(0, steps - 1, nextVm)
        }
      }
    }

    helper(0, steps, initVM)
  }
}

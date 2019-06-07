package FiveStage
import cats.data.Writer
import cats._
import cats.data.{ Op => _ }
import cats.implicits._

import fileUtils.say

object DeletDis {

  def delet = {

    case class Fix[F[_]](unfix: F[Fix[F]])
    case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]]){
      def counit: A = head
      def map[B](f: A => B)(implicit ev: Functor[F]): Cofree[F, B] = Cofree(f(head), tail.map(_.map(f)))
    
      /**
        * Coflatmap alters the value of the node based on its context, then recursively
        * alters its tail independently (which makes sense as it's the only thing Cofree[F, A] => B can do.
        */
      def coflatMap[B](fa: Cofree[F, A] => B)(implicit ev: Functor[F]): Cofree[F, B] = {
        val b = fa(this)
        val fb = tail.map(_.coflatMap(fa))
        Cofree(b, fb)
      }
    }
  
    // Anamorphism: Generalized unfold, builds structures
    def ana[F[_]: Functor, A](f: A => F[A])(a: A): Fix[F] =
      Fix( (f(a)).map(ana(f)) )
    
    // Catamorphism: Generalized fold, tears structures down.
    def cata[F[_]: Functor, A](fa: F[A] => A)(f: Fix[F]): A = {
      fa(f.unfix.map(cata(fa)))
    }
    
    // def hyloSimple[F[_] : Functor, A, B](f: F[B] => B)(g: A => F[A]): A => B
    def hylo[F[_]: Functor, A, B](f: F[B] => B)(g: A => F[A])(a: A): B =
      cata(f)(ana(g)(a))
    
    // A more powerful cata
    def para[F[_]: Functor, A](f: F[(Fix[F], A)] => A)(fa: Fix[F]): A =
      f(fa.unfix.map(x => (x, para(f)(x))))
    
    // A more powerful ana
    def apo[F[_]: Functor, A](f: A => F[Either[Fix[F], A]])(a: A): Fix[F] = {
      Fix(f(a).map{
        case Right(a) => apo(f)(a)
        case Left(fix) => fix
      })
    }
    
    // When we have cofree
    def histo[F[_]: Functor, A](f: F[Cofree[F, A]] => A)(fix: Fix[F]): A = {
      def toCofree(fix: Fix[F]): Cofree[F, A] =
        Cofree(histo(f)(fix), fix.unfix.map(toCofree))
    
      f(fix.unfix.map(toCofree))
    }
  
    sealed trait StackR
    final case class DoneR(result: Int = 1) extends StackR
    final case class MoreR(stack: StackR, next: Int) extends StackR
  
    def unfoldStackR(n: Int): StackR =
      if(n > 0) MoreR(unfoldStackR(n-1), n) else DoneR()
  
    say(unfoldStackR(5))

    sealed trait Stack[A]
    final case class Done[A](result: Int) extends Stack[A]
    final case class More[A](a: A, next: Int) extends Stack[A]

    object Stack {
      implicit val stackFunctor: Functor[Stack] = new Functor[Stack] {
        def map[A, B](fa: Stack[A])(f: A => B): Stack[B] = fa match {
          case Done(result) => Done(result)
          case More(a, next) => More(f(a), next)
        }
      }

      def done[A](result: Int = 1): Stack[A] = Done(result)
      def more[A](a: A, next: Int): Stack[A] = More(a, next)
    }

    import Stack._

    val stackCoalgebra: Int => Stack[Int] =
      n => if(n > 0) more(n - 1, n) else done()

    say(ana(stackCoalgebra)(5))

    val stackAlgebra: Stack[Int] => Int = {
      case Done(result) => result
      case More(acc, next) => acc * next
    }

    say(cata(stackAlgebra)(ana(stackCoalgebra)(5)))
    say(hylo(stackAlgebra)(stackCoalgebra)(5))


    sealed trait Nat[A]
    final case class Zero[A]() extends Nat[A]
    final case class Succ[A](prev: A) extends Nat[A]
    
    object Nat {
      implicit val natFunctor: Functor[Nat] = new Functor[Nat] {
        override def map[A, B](na: Nat[A])(f: A => B): Nat[B] =
          na match {
            case Zero() => Zero()
            case Succ(a) => Succ(f(a))
          }
      }
    }

    val natAlgebra: Nat[Int] => Int = {
      case Zero() => 1
      case Succ(n) => {
        say(s"nat alg succ $n")
        n + 1
      }
    }

    val natAlgebraS: Nat[String] => String = {
      case Zero() => "N"
      case Succ(n) => n match {
        case "N" => "NI"
        case "NI" => "NIG"
        case "NIG" => "NIGG"
        case "NIGG" => "NIGGE"
        case "NIGGE" => "NIGGER :-"
        case s => s + "D"
      }
    }

    val natCoalgebra: Int => Nat[Int] =
      n => if (n == 0) Zero() else Succ(n - 1)

    val b = ana(natCoalgebra)(9)
    val c = cata(natAlgebraS)(b)
    say(c)


    val natAlgebraPara: Nat[(Fix[Nat], Int)] => Int = {
      case Zero() => 1
      case Succ((fix, acc)) => {
        say(s"nat para alg succ $fix, $acc")
        cata(natAlgebra)(fix) * acc
      }
    }

    val build = ana(natCoalgebra)(_)
    say("built")
    val tear = para(natAlgebraPara)(_)
    say(tear(build(5)))
    // say(ana(natCoalgebra)(5))

    val lastThreeSteps: Fix[Stack] = Fix(More(Fix(More(Fix(More(Fix(Done(1)),1)),2)),3))

    val stackCoalgebraApo: Int => Stack[Either[Fix[Stack], Int]] =
      n => if(n > 3) more(n - 1, n).map(_.asRight) else lastThreeSteps.unfix.map(_.asLeft)
  }
}

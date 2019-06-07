package FiveStage
import cats.data.Writer
import cats._
import cats.data.{ Op => _ }
import cats.implicits._

object DTree {

  // opaques WHEN
  type Feature = String
  type Value = String
  type Cls = String

  case class TrainingData(values: Map[Feature, Value], cls: Cls)

  type TestData = Map[Feature, Value]

  // Base
  // def predict(data: TestData): Cls = "Died"

  // Gendered
  def predictStatic(data: TestData): Cls =
    if(data("gender") == "female")
      "survived"
    else "died"


  sealed trait Tree
  case class Node(feature: Feature, children: Map[Value, Tree]) extends Tree
  case class Leaf(cls: Cls) extends Tree

  val genderBased: Tree = Node(
    "gender", Map(
      "female" -> Leaf("survived"),
      "male" -> Leaf("died"),
    ))


  def predict(data: TestData)(tree: Tree): Cls =
    tree match {
      case Leaf(cls) => cls
      case Node(feature, children) => predict(data)(children(data(feature)))
    }

  val data = Map("gender" -> "female", "family size" -> "0", "ticket" -> "1")

  predict(data)(genderBased) // true


  def entropy(classes: List[Cls]): Double = {
    val total = classes.size
    classes.groupBy(identity)
      .mapValues { group =>
        val prop = group.size / total
        prop * math.log(1.0 / prop)
      }.values.sum
  }

  def bucketedEntropy(data: List[TrainingData], feature: Feature): Double = {
    val total = data.size
    val bucketed = data.groupBy(_.values(feature))
      .mapValues(_.map(_.cls))
      .toMap
    bucketed.values.map { classes =>
      val prop = classes.size / total
      prop * entropy(classes)
    }.sum
  }

  def best(data: List[TrainingData], features: Set[Feature]): Feature = features.minBy(bucketedEntropy(data, _))

  def mostCommonCls(data: List[TrainingData]): Cls = ???

  def build(data: List[TrainingData], features: Set[Feature]): Tree = {
    if(features.nonEmpty) {
      val feature = best(data, features)
      val buckets = data.groupBy(_.values(feature))
      Node(feature, buckets.mapValues(build(_, features - feature)))
    } else {
      Leaf(mostCommonCls(data))
    }
  }

  def withHKT {

    sealed trait Tree[A]
    case class Node[A](feature: Feature, children: Map[Value, A]) extends Tree[A]
    case class Leaf[A](cls: Cls) extends Tree[A]

    // import matryoshka._
    // import matryoshka.data.Fix
    // import matryoshka.implicits._

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

    implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
      def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Node(name, children) => Node(name, children.mapValues(f))
        case Leaf(cls) => Leaf(cls)
      }
    }

    val genderBased: Fix[Tree] =
      Fix(Node(
        "gender",
        Map(
          "female" -> Fix(Leaf[Fix[Tree]]("survived")),
          "male"   -> Fix(Leaf[Fix[Tree]]("died"))
        )))

    def build: ((List[TrainingData], Set[Feature])) => Tree[(List[TrainingData], Set[Feature])] = {
      case (data, features) =>
        if(features.nonEmpty) {
          val feature = best(data, features)
          val buckets = data.groupBy(_.values(feature))
          val next = buckets.mapValues { subset => (subset, features - feature) }
          Node(feature, next)
        } else {
          Leaf(mostCommonCls(data))
        }
    }

    def explore(testData: TestData): Fix[Tree] => Cls Either Fix[Tree] =
      fix => fix.unfix match {
        case Leaf(cls) => Left(cls)
        case Node(feature, children) => Right(children.get(testData(feature)).get)
      }

    // Anamorphism: Generalized unfold, builds structures
    def ana[F[_]: Functor, A](f: A => F[A])(a: A): Fix[F] =
      Fix( (f(a)).map(ana(f)) )

    // Catamorphism: Generalized fold, tears structures down.
    def cata[F[_]: Functor, A](fa: F[A] => A)(f: Fix[F]): A = {
      fa(f.unfix.map(cata(fa)))
    }

    // def hyloSimple[F[_] : Functor, A, B](f: F[B] => B)(g: A => F[A]): A => B
    def hyloSimple[F[_]: Functor, A, B](f: F[B] => B)(g: A => F[A])(a: A): B =
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


  }
}

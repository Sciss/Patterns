package de.sciss.patterns

import scala.collection.AbstractIterator
import scala.language.implicitConversions

object Types {
  def main(args: Array[String]): Unit = example()

  trait Top {
    type Out
  }

  trait Num[T1 <: Top, T2 <: Top, T <: Top] {
    val tpe: T

    def lift1(a: T1#Out): tpe.Out
    def lift2(a: T2#Out): tpe.Out

    def plus(a: tpe.Out, b: tpe.Out): tpe.Out
  }

  trait Elem[T <: Top] {
    val tpe: T

    def iterator: Iterator[tpe.Out]
  }

  sealed trait IntLikeTop extends Top

  sealed trait IntSeqTop extends IntLikeTop {
    type Out = Seq[Int]
  }
  implicit object IntSeqTop extends IntSeqTop

  sealed trait IntTop extends IntLikeTop {
    override type Out = Int
  }
  object IntTop extends IntTop

//  sealed trait DoubleSeqTop extends NumTop /* DoubleLikeTop */ {
//    def bridge(a: NumTop, b: NumTop): Bridge[a.Out, b.Out, Out] = ...
//  }
//  implicit object DoubleSeqTop extends DoubleSeqTop
//
//  sealed trait DoubleTop extends DoubleSeqTop

  implicit object intNum extends Num[IntTop, IntTop, IntTop] {
    val tpe: IntTop = IntTop

    def lift1(a: Int): Int = a
    def lift2(a: Int): Int = a

    def plus(a: Int, b: Int): Int = a + b
  }

  trait IntLikeNum {
    final val tpe: IntSeqTop = IntSeqTop

    final def plus(a: Seq[Int], b: Seq[Int]): Seq[Int] = {
      val as = a.size
      val bs = b.size
      val sz = math.max(as, bs)
      Seq.tabulate(sz) { i =>
        a(i % as) + b(i % bs)
      }
    }
  }

  implicit object intSeqNum1 extends IntLikeNum with Num[IntTop, IntSeqTop, IntSeqTop] {
    def lift1(a: Int     ): Seq[Int] = a :: Nil
    def lift2(a: Seq[Int]): Seq[Int] = a
  }

  implicit object intSeqNum2 extends IntLikeNum with Num[IntSeqTop, IntTop, IntSeqTop] {
    def lift1(a: Seq[Int]): Seq[Int] = a
    def lift2(a: Int     ): Seq[Int] = a :: Nil
  }

  implicit object intSeqNum3 extends IntLikeNum with Num[IntSeqTop, IntSeqTop, IntSeqTop] {
    def lift1(a: Seq[Int]): Seq[Int] = a
    def lift2(a: Seq[Int]): Seq[Int] = a
  }

  final case class Add[T1 <: Top, T2 <: Top, T <: Top](a: Elem[T1], b: Elem[T2])
                                                      (implicit val br: Num[T1, T2, T])
    extends Elem[T] {

    val tpe: br.tpe.type = br.tpe

    def iterator: Iterator[tpe.Out] = {
      val ai = a.iterator
      val bi = b.iterator

      new AbstractIterator[tpe.Out] {
        def hasNext: Boolean = ai.hasNext && bi.hasNext

        def next(): tpe.Out = {
          val an  = br.lift1(ai.next())
          val bn  = br.lift2(bi.next())
          val res = br.plus(an, bn)
          res
        }
      }
    }
  }

  final case class Series[T1 <: Top, T2 <: Top, T <: Top](list: Elem[T1], step: Elem[T2])
                                                         (implicit val br: Num[T1, T2, T])
    extends Elem[T] {

    val tpe: br.tpe.type = br.tpe

    def iterator: Iterator[tpe.Out] = {
      val ai = list.iterator
      val bi = step.iterator

      if (ai.isEmpty) Iterator.empty
      else new AbstractIterator[tpe.Out] {
        private var state: tpe.Out = br.lift1(ai.next)
        var hasNext = true

        def next(): tpe.Out = {
          val res = state
          hasNext = ai.hasNext && bi.hasNext
          if (hasNext) {
            state = br.plus(state, br.lift2(bi.next()))
          }
          res
        }
      }
    }
  }

  final case class Cat[T1 <: Top, T2 <: Top, T <: Top](a: Elem[T1], b: Elem[T2])(implicit val tpe: T) extends Elem[T] {
    def iterator: Iterator[tpe.Out] = ??? // a.mkIter ++ b.mkIter
  }

  sealed trait StringTop extends Top {
    type Out = String
  }
  implicit object StringTop extends StringTop

  final case class ConstInt(x: Int) extends Elem[IntTop] {
    val tpe: IntTop = IntTop

    def iterator: Iterator[tpe.Out] = Iterator.continually(x)
  }

  final case class ConstIntSeq(xs: Seq[Int]) extends Elem[IntSeqTop] {
    val tpe: IntSeqTop = IntSeqTop

    def iterator: Iterator[tpe.Out] = Iterator.continually(xs)
  }

  final case class ConstString(x: String) extends Elem[StringTop] {
    val tpe: StringTop = StringTop

    def iterator: Iterator[tpe.Out] = Iterator.continually(x)
  }

  implicit def intElem      (x: Int         ): Elem[IntTop      ] = ConstInt(x)
  implicit def intSeqElem   (xs: Seq[Int]   ): Elem[IntSeqTop   ] = ConstIntSeq(xs)
//  implicit def doubleElem   (i: Double      ): Elem[DoubleTop   ] = ...
//  implicit def doubleSeqElem(i: Seq[Double] ): Elem[DoubleSeqTop] = ...
  implicit def stringElem   (x: String      ): Elem[StringTop   ] = ConstString(x)

  def example(): Unit = {
    // compiles
    val a = Add[IntSeqTop, IntTop, IntSeqTop](Seq(1, 2), 3)
    println(a.iterator.take(1).mkString("a: ", ", ", ""))

    // compiles
    val b = Add(Seq(1, 2), 3)
    println(b.iterator.take(1).mkString("b: ", ", ", ""))

    // compiles
    val c = Add(1, 3)
    c.iterator: Iterator[Int] // right
    println(c.iterator.take(1).mkString("c: ", ", ", ""))

    // compiles
    val d = Add(1, Seq(2, 3))
    println(d.iterator.take(1).mkString("d: ", ", ", ""))

    // compiles
    val e = Add(Seq(1, 2), Seq(3, 4))
    println(e.iterator.take(1).mkString("e: ", ", ", ""))

    // compiles
    val f = Series(Seq(2, 3), 4)
    println(f.iterator.take(3).mkString("f: ", ", ", ""))

    //    Cat("foo", "bar")

//    // ambiguous implicits
//    Foo(Seq(1, 2), 3.0)

    //    // compiles not
//    Foo(Seq(1, 2), "foo")
  }
}

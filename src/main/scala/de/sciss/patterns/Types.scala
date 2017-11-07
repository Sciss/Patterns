package de.sciss.patterns

import scala.collection.AbstractIterator
import scala.language.implicitConversions

object Types {
  trait Top {
    type Out
  }

  trait Bridge[In1, In2, Out] {
    def plus(a: In1, b: In2): Out
  }

  trait Elem[+T <: Top] {
    val tpe: T

    def mkIter: Iterator[tpe.Out]
  }

  sealed trait NumTop extends Top {
//    def num: Numeric[A]

//    def bridge[In1 <: A, In2 <: A](a: Top[In1], b: Top[In2]): Bridge[In1, In2, A]
    def bridge(a: NumTop, b: NumTop): Bridge[a.Out, b.Out, Out]

//    def plus(x: A, y: A): A
  }

  sealed trait IntLikeTop extends NumTop {
    def bridge(a: NumTop, b: NumTop): Bridge[a.Out, b.Out, Out] = ???
  }

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

  final case class Add[T <: NumTop](a: Elem[T], b: Elem[T])(implicit val tpe: T) extends Elem[T] {
    def mkIter: Iterator[tpe.Out] = {
      val ai = a.mkIter
      val bi = b.mkIter
      val br = tpe.bridge(a.tpe, b.tpe)

      new AbstractIterator[tpe.Out] {
        def hasNext: Boolean = ai.hasNext && bi.hasNext

        def next(): tpe.Out = {
          val an = ai.next()
          val bn = bi.next()
          br.plus(an, bn)
        }
      }
    }
  }

  final case class Cat[T <: Top, T1 <: T, T2 <: T](a: Elem[T1], b: Elem[T2])(implicit val tpe: T) extends Elem[T] {
    def mkIter: Iterator[tpe.Out] = ??? // a.mkIter ++ b.mkIter
  }

  sealed trait StringTop extends Top {
    type Out = String
  }
  implicit object StringTop extends StringTop

  final case class ConstInt(x: Int) extends Elem[IntTop] {
    val tpe: IntTop = IntTop

    def mkIter: Iterator[tpe.Out] = Iterator.continually(x)
  }

  final case class ConstIntSeq(xs: Seq[Int]) extends Elem[IntSeqTop] {
    val tpe: IntSeqTop = IntSeqTop

    def mkIter: Iterator[tpe.Out] = Iterator.continually(xs)
  }

  final case class ConstString(x: String) extends Elem[StringTop] {
    val tpe: StringTop = StringTop

    def mkIter: Iterator[tpe.Out] = Iterator.continually(x)
  }

  implicit def intElem      (x: Int         ): Elem[IntTop      ] = ConstInt(x)
  implicit def intSeqElem   (xs: Seq[Int]   ): Elem[IntSeqTop   ] = ConstIntSeq(xs)
//  implicit def doubleElem   (i: Double      ): Elem[DoubleTop   ] = ...
//  implicit def doubleSeqElem(i: Seq[Double] ): Elem[DoubleSeqTop] = ...
  implicit def stringElem   (x: String      ): Elem[StringTop   ] = ConstString(x)

  def example(): Unit = {
    // compiles
    Add(Seq(1, 2), 3)

    Cat("foo", "bar")

//    // ambiguous implicits
//    Foo(Seq(1, 2), 3.0)

    //    // compiles not
//    Foo(Seq(1, 2), "foo")
  }
}

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
    def bridge(a: Top, b: Top): Bridge[a.Out, b.Out, Out]

//    def plus(x: A, y: A): A
  }

//  sealed trait IntLikeTop extends NumTop {
////    def bridge[In1 <: A, In2 <: A](a: Top[In1], b: Top[In2]): Bridge[In1, In2, A] = ???
//  }

  sealed trait IntSeqTop extends NumTop /* IntLikeTop */ {
//    final type Out = Seq[Int]
//    def num: Numeric[Out] = ???
//    def plus(x: Out, y: Out): Out = (x, y).zipped.map(_ + _)
    def bridge(a: Top, b: Top): Bridge[a.Out, b.Out, Out] = ???
  }
  implicit object IntSeqTop extends IntSeqTop

  sealed trait IntTop extends IntSeqTop /* IntLikeTop */ {
    //    def num: Numeric[Out] = ???
//    final type Out = Int
    //    def plus(x: Out, y: Out): Out = x + y
  }
//  implicit object IntTop extends IntTop

  final case class Foo[T <: NumTop](a: Elem[T], b: Elem[T])(implicit val tpe: T) extends Elem[T] {
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

  implicit def intElem   (i: Int     ): Elem[IntTop   ] = ???
  implicit def intSeqElem(i: Seq[Int]): Elem[IntSeqTop] = ???

  def example(): Unit = {
    Foo(Seq(1, 2), 3)
  }
}

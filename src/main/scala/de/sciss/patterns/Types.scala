package de.sciss.patterns

import scala.collection.AbstractIterator

object Types {
  trait Top {
    type Out
  }

  trait Elem[A <: Top] {
    def mkIter: Iterator[A#Out]
  }

  sealed trait NumTop[Out1] extends Top {
    final type Out = Out1
    def num: Numeric[Out]
  }

  sealed trait IntLikeTop[Out1] extends NumTop[Out1]

  sealed trait IntTop extends IntLikeTop[Int] {
    def num: Numeric[Out] = ???
  }
  implicit object IntTop extends IntTop

  sealed trait IntSeqTop extends IntLikeTop[Seq[Int]] {
    def num: Numeric[Out] = ???
  }
  implicit object IntSeqTop extends IntSeqTop

  final case class Foo[A, T <: NumTop[A]](a: Elem[T], b: Elem[T])(implicit tpe: T) extends Elem[T] {
    def mkIter: Iterator[A] = {
      val ai = a.mkIter
      val bi = b.mkIter

      new AbstractIterator[A] {
        def hasNext: Boolean = ai.hasNext && bi.hasNext

        def next(): A = {
          val an = ai.next()
          val bn = bi.next()
          tpe.num.plus(an, bn)
        }
      }
    }
  }

  implicit def intElem    (i: Int     ): Elem[IntTop]     = ???
  implicit def intSeqElem (i: Seq[Int]): Elem[IntSeqTop]  = ???

  def example(): Unit = {
    Foo(Seq(1, 2), 3)
  }
}

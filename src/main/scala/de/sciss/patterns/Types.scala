package de.sciss.patterns

import scala.collection.AbstractIterator
import scala.language.implicitConversions

object Types {
  trait Top[A] {
//    type Out
  }

  trait Bridge[In1, In2, Out] {
    def plus(a: In1, b: In2): Out
  }

  trait Elem[A, +T <: Top[A]] {
    val tpe: T

    def mkIter: Iterator[A]
  }

  sealed trait NumTop[A] extends Top[A] {
//    def num: Numeric[A]

    def bridge[In1 <: A, In2 <: A](a: Top[In1], b: Top[In2]): Bridge[In1, In2, A]

//    def plus(x: A, y: A): A
  }

  sealed trait IntLikeTop[A] extends NumTop[A] {
    def bridge[In1 <: A, In2 <: A](a: Top[In1], b: Top[In2]): Bridge[In1, In2, A] = ???
  }

  sealed trait IntTop extends IntLikeTop[Int] {
//    def num: Numeric[Out] = ???
    final type Out = Int
//    def plus(x: Out, y: Out): Out = x + y
  }
  implicit object IntTop extends IntTop

  sealed trait IntSeqTop extends IntLikeTop[Seq[Int]] {
    final type Out = Seq[Int]
//    def num: Numeric[Out] = ???
//    def plus(x: Out, y: Out): Out = (x, y).zipped.map(_ + _)
  }
  implicit object IntSeqTop extends IntSeqTop

  final case class Foo[A, T <: NumTop[A]](a: Elem[A, T], b: Elem[A, T])(implicit val tpe: T) extends Elem[A, T] {
    def mkIter: Iterator[A] = {
      val ai = a.mkIter
      val bi = b.mkIter
      val br = tpe.bridge(a.tpe, b.tpe)

      new AbstractIterator[A] {
        def hasNext: Boolean = ai.hasNext && bi.hasNext

        def next(): A = {
          val an = ai.next()
          val bn = bi.next()
          br.plus(an, bn)
        }
      }
    }
  }

  implicit def intElem    (i: Int     ): Elem[Int     , IntTop   ] = ???
  implicit def intSeqElem (i: Seq[Int]): Elem[Seq[Int], IntSeqTop] = ???

  def example(): Unit = {
    // Foo(Seq(1, 2), 3)
  }
}

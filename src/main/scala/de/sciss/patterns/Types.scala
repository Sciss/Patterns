/*
 *  Types.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns

import java.lang.{String => _String}

object Types {
  trait Top {
    type Out
  }

  trait Bridge[T1 <: Top, T2 <: Top, T <: Top] {
    val tpe: T

    def lift1(a: T1#Out): tpe.Out
    def lift2(a: T2#Out): tpe.Out
  }

  trait Num[T1 <: Top, T2 <: Top, T <: Top] extends Bridge[T1, T2, T] {
    def plus (a: tpe.Out, b: tpe.Out): tpe.Out
    def times(a: tpe.Out, b: tpe.Out): tpe.Out
  }

  trait IntLikeNum {
    final val tpe: IntSeqTop = IntSeqTop

    final def plus (a: Seq[Int], b: Seq[Int]): Seq[Int] = combine(a, b)(_ + _)
    final def times(a: Seq[Int], b: Seq[Int]): Seq[Int] = combine(a, b)(_ * _)

    private def combine(a: Seq[Int], b: Seq[Int])(op: (Int, Int) => Int): Seq[Int] = {
      val as = a.size
      val bs = b.size
      val sz = math.max(as, bs)
      Seq.tabulate(sz) { i =>
        op(a(i % as), b(i % bs))
      }
    }
  }

  sealed trait IntLikeTop extends Top {
    def lift(in: Out): Seq[Int]
  }

  sealed trait IntSeqTop extends IntLikeTop {
    type Out = Seq[Int]
  }
  implicit object IntSeqTop extends IntSeqTop with IntLikeNum with Num[IntSeqTop, IntSeqTop, IntSeqTop] {
    def lift(in: Seq[Int]): Seq[Int] = in

    def lift1(a: Seq[Int]): Seq[Int] = a
    def lift2(a: Seq[Int]): Seq[Int] = a
  }

  sealed trait IntTop extends IntLikeTop {
    override type Out = Int
  }
  implicit object IntTop extends IntTop with Num[IntTop, IntTop, IntTop] {
    val tpe: IntTop = this

    def lift(in: Int): Seq[Int] = in :: Nil

    def lift1(a: Int): Int = a
    def lift2(a: Int): Int = a

    def plus (a: Int, b: Int): Int = a + b
    def times(a: Int, b: Int): Int = a * b
  }

//  sealed trait DoubleSeqTop extends NumTop /* DoubleLikeTop */ {
//    def bridge(a: NumTop, b: NumTop): Bridge[a.Out, b.Out, Out] = ...
//  }
//  implicit object DoubleSeqTop extends DoubleSeqTop
//
//  sealed trait DoubleTop extends DoubleSeqTop


  implicit object intSeqNum1 extends IntLikeNum with Num[IntTop, IntSeqTop, IntSeqTop] {
    def lift1(a: Int     ): Seq[Int] = a :: Nil
    def lift2(a: Seq[Int]): Seq[Int] = a
  }

  implicit object intSeqNum2 extends IntLikeNum with Num[IntSeqTop, IntTop, IntSeqTop] {
    def lift1(a: Seq[Int]): Seq[Int] = a
    def lift2(a: Int     ): Seq[Int] = a :: Nil
  }

  ////////////////////////////

  sealed trait StringTop extends Top {
    type Out = _String
  }
  implicit object StringTop extends StringTop with Bridge[StringTop, StringTop, StringTop] {
    val tpe: StringTop = this

    def lift1(a: _String): _String = a
    def lift2(a: _String): _String = a
  }
}

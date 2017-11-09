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

import scala.language.higherKinds

object Types {
  type TopT[A] = Top { type Out = A }

  object Top {
    type Seq[T <: Top] = Top { type Out = scala.Seq[T#Out] }

//    def Seq[T <: Top](implicit peer: T): Seq[T] = new Seq(peer)
//    final class Seq[T <: Top](val peer: T) extends Top {
//      type Out = scala.Seq[T#Out]
//    }
  }
  trait Top {
    type Out

//    def lift(x: Out): Out
  }

  trait Bridge[T1 <: Top, T2 <: Top, T <: Top] {
    def lift1(a: T1#Out): T#Out
    def lift2(a: T2#Out): T#Out
  }

  trait Num[T1 <: Top, T2 <: Top, T <: Top] extends Bridge[T1, T2, T] {
    def plus (a: T#Out, b: T#Out): T#Out
    def times(a: T#Out, b: T#Out): T#Out
  }
  
  trait SeqLikeNum[A] {
    protected val num: Numeric[A]
    
    import num._

    final def plus (a: Seq[A], b: Seq[A]): Seq[A] = combine(a, b)(_ + _)
    final def times(a: Seq[A], b: Seq[A]): Seq[A] = combine(a, b)(_ * _)
    
    protected def combine(a: Seq[A], b: Seq[A])(op: (A, A) => A): Seq[A] = {
      val as = a.size
      val bs = b.size
      val sz = math.max(as, bs)
      Seq.tabulate(sz) { i =>
        op(a(i % as), b(i % bs))
      }
    }
  }

  trait IntLikeNum extends SeqLikeNum[Int] {
    protected val num: Numeric[Int] = Numeric.IntIsIntegral
  }

  trait DoubleLikeNum extends SeqLikeNum[Double] {
    protected val num: Numeric[Double] = Numeric.DoubleIsFractional
  }

  object Applicative {
    implicit object Iterator extends Applicative[scala.Iterator] {
      def pure[A](x: A): scala.Iterator[A] = scala.Iterator.single(x)

      def map[A, B](fa: Iterator[A])(f: A => B): Iterator[B] = fa.map(f)

      def flatMap[A, B](fa: Iterator[A])(f: A => Iterator[B]): Iterator[B] =
        fa.flatMap(f)
    }
  }
  // XXX TODO -- a bit ad-hoc; we could just depend on Cats
  trait Applicative[F[_]] {
    def pure[A](x: A): F[A]

    def map    [A, B](fa: F[A])(f: A =>   B ): F[B]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def ap     [A, B   ](ff: F[A => B])(fa: F[A]): F[B]             = flatMap(ff)(f => map(fa)(f))
    def product[A, B   ](fa: F[A], fb: F[B]): F[(A, B)]             = ap(map(fa)(a => (b: B) => (a, b)))(fb)
    def map2   [A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z]  = map(product(fa, fb)) { case (a, b) => f(a, b) }
  }

  trait ScalarOrSeqTop[A] {
    def lift(in: Out): Seq[A]

    type Index[_]
    type Out = Index[A]

    def mapIndex[B, C](i: Index[B])(fun: B => C): Index[C]

    def traverseIndex[G[_], B, C](fa: Index[B])(f: B => G[C])(implicit app: Applicative[G]): G[Index[C]]
  }

  trait SeqTop[A] extends ScalarOrSeqTop[A] {
    type Index[B] = Seq[B]

    final def lift(in: Seq[A]): Seq[A] = in

    final def lift1(a: Seq[A]): Seq[A] = a
    final def lift2(a: Seq[A]): Seq[A] = a

    final def mapIndex[B, C](i: Index[B])(fun: B => C): Index[C] = i.map(fun)

    final def traverseIndex[G[_], B, C](fa: Index[B])(f: B => G[C])(implicit app: Applicative[G]): G[Index[C]] =
      fa.foldRight[G[Index[C]]](app.pure(Seq.empty)) { (a, lglb) =>
        app.map2(f(a), lglb)(_ +: _)
      }
  }

  trait ScalarTop[A] extends ScalarOrSeqTop[A] {
    type Index[B] = B

    final def lift(in: A): Seq[A] = in :: Nil

    final def lift1(a: A): A = a
    final def lift2(a: A): A = a

    final def mapIndex[B, C](i: Index[B])(fun: B => C): Index[C] = fun(i)

    final def traverseIndex[G[_], B, C](fa: Index[B])(f: B => G[C])(implicit app: Applicative[G]): G[Index[C]] = f(fa)
  }

  sealed trait IntLikeTop extends ScalarOrSeqTop[Int] with Top

  sealed trait IntSeqTop extends IntLikeTop with SeqTop[Int]
  implicit object IntSeqTop extends IntSeqTop with IntLikeNum with Num[IntSeqTop, IntSeqTop, IntSeqTop]

  sealed trait IntTop extends IntLikeTop with ScalarTop[Int]
  implicit object IntTop extends IntTop with Num[IntTop, IntTop, IntTop] {
    def plus (a: Int, b: Int): Int = a + b
    def times(a: Int, b: Int): Int = a * b
  }

  sealed trait DoubleLikeTop extends ScalarOrSeqTop[Double] with Top

  sealed trait DoubleSeqTop extends DoubleLikeTop with SeqTop[Double]
  implicit object DoubleSeqTop extends DoubleSeqTop with DoubleLikeNum with Num[DoubleSeqTop, DoubleSeqTop, DoubleSeqTop]

  sealed trait DoubleTop extends DoubleLikeTop with ScalarTop[Double]
  implicit object DoubleTop extends DoubleTop with Num[DoubleTop, DoubleTop, DoubleTop] {
    def plus (a: Double, b: Double): Double = a + b
    def times(a: Double, b: Double): Double = a * b
  }
  
  implicit object intSeqNum1 extends IntLikeNum with Num[IntTop, IntSeqTop, IntSeqTop] {
    def lift1(a: Int     ): Seq[Int] = a :: Nil
    def lift2(a: Seq[Int]): Seq[Int] = a
  }

  implicit object intSeqNum2 extends IntLikeNum with Num[IntSeqTop, IntTop, IntSeqTop] {
    def lift1(a: Seq[Int]): Seq[Int] = a
    def lift2(a: Int     ): Seq[Int] = a :: Nil
  }

  implicit object doubleSeqNum1 extends DoubleLikeNum with Num[DoubleTop, DoubleSeqTop, DoubleSeqTop] {
    def lift1(a: Double     ): Seq[Double] = a :: Nil
    def lift2(a: Seq[Double]): Seq[Double] = a
  }

  implicit object doubleSeqNum2 extends DoubleLikeNum with Num[DoubleSeqTop, DoubleTop, DoubleSeqTop] {
    def lift1(a: Seq[Double]): Seq[Double] = a
    def lift2(a: Double     ): Seq[Double] = a :: Nil
  }

  ////////////////////////////

  sealed trait StringTop extends Top {
    type Out = _String
  }
  implicit object StringTop extends StringTop with Bridge[StringTop, StringTop, StringTop] {
    def lift1(a: _String): _String = a
    def lift2(a: _String): _String = a
  }
}

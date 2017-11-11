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

import de.sciss.numbers.{DoubleFunctions, IntFunctions}

import scala.language.higherKinds
import scala.math.Numeric.{DoubleIsFractional, IntIsIntegral}
import scala.math.Ordering
import scala.util.Random

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

  trait Num[A] {
    def plus  (x: A, y: A): A
    def minus (x: A, y: A): A
    def times (x: A, y: A): A

    def negate(x: A): A
    def abs   (x: A): A

    def zero: A
    def one : A

    def rand2(a: A      )(implicit r: Random): A
    def rrand(a: A, b: A)(implicit r: Random): A

    def fold(a: A, lo: A, hi: A)(implicit r: Random): A
  }
  
  trait SeqLikeNum[A] extends Num[Seq[A]] {
    protected val peer: Num[A]
    
    final def plus (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.plus )
    final def minus(a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.minus)
    final def times(a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.times)

    def negate(a: Seq[A]): Seq[A] = unOp(a)(peer.negate)
    def abs   (a: Seq[A]): Seq[A] = unOp(a)(peer.abs   )

    def zero: Seq[A] = peer.zero :: Nil
    def one : Seq[A] = peer.one  :: Nil

    def rand2(a: Seq[A]           )(implicit r: Random): Seq[A] = unOp (a   )(peer.rand2)
    def rrand(a: Seq[A], b: Seq[A])(implicit r: Random): Seq[A] = binOp(a, b)(peer.rrand)

    def fold(a: Seq[A], lo: Seq[A], hi: Seq[A])(implicit r: Random): Seq[A] = ternOp(a, lo, hi)(peer.fold)

    final protected def unOp(a: Seq[A])(op: A => A): Seq[A] = a.map(op)

    final protected def binOp(a: Seq[A], b: Seq[A])(op: (A, A) => A): Seq[A] = {
      val as = a.size
      val bs = b.size
      val sz = math.max(as, bs)
      Seq.tabulate(sz) { i =>
        op(a(i % as), b(i % bs))
      }
    }

    final protected def ternOp(a: Seq[A], b: Seq[A], c: Seq[A])(op: (A, A, A) => A): Seq[A] = {
      val as = a.size
      val bs = b.size
      val cs = c.size
      val sz = math.max(math.max(as, bs), cs)
      Seq.tabulate(sz) { i =>
        op(a(i % as), b(i % bs), c(i % cs))
      }
    }
  }

  trait IntLikeNum extends SeqLikeNum[Int] {
    protected final val peer: Num[Int] = IntTop
  }

  trait DoubleLikeNum extends SeqLikeNum[Double] {
    protected final val peer: Num[Double] = DoubleTop
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
  implicit object IntSeqTop
    extends IntSeqTop 
      with  IntLikeNum
      with  Bridge[IntSeqTop, IntSeqTop, IntSeqTop] 
      with  Num[Seq[Int]]

  sealed trait IntTop extends IntLikeTop with ScalarTop[Int]
  implicit object IntTop 
    extends IntTop 
      with  Bridge[IntTop, IntTop, IntTop] 
      with  Num[Int] 
      with  IntIsIntegral 
      with  Ordering.IntOrdering {
    
    def rand2(a: Int)(implicit r: Random): Int = r.nextInt(2 * a + 1) - a

    def rrand(a: Int, b: Int)(implicit r: Random): Int = r.nextInt(b - a + 1) + a

    def fold(a: Int, lo: Int, hi: Int)(implicit r: Random): Int = IntFunctions.fold(a, lo, hi)
  }

  sealed trait DoubleLikeTop extends ScalarOrSeqTop[Double] with Top

  sealed trait DoubleSeqTop extends DoubleLikeTop with SeqTop[Double]
  implicit object DoubleSeqTop
    extends DoubleSeqTop
      with  DoubleLikeNum
      with  Bridge[DoubleSeqTop, DoubleSeqTop, DoubleSeqTop]
      with  Num[Seq[Double]]

  sealed trait DoubleTop extends DoubleLikeTop with ScalarTop[Double]
  implicit object DoubleTop
    extends DoubleTop
      with  Bridge[DoubleTop, DoubleTop, DoubleTop]
      with  Num[Double]
      with  DoubleIsFractional
      with  Ordering.DoubleOrdering {

    def rand2(a: Double)(implicit r: Random): Double = (r.nextDouble() * 2 - 1) * a

    def rrand(a: Double, b: Double)(implicit r: Random): Double = r.nextDouble() * (b - a) + a

    def fold(a: Double, lo: Double, hi: Double)(implicit r: Random): Double = DoubleFunctions.fold(a, lo, hi)
  }
  
  implicit object intSeqBridge1 extends /* IntLikeNum with */ Bridge[IntTop, IntSeqTop, IntSeqTop] {
    def lift1(a: Int     ): Seq[Int] = a :: Nil
    def lift2(a: Seq[Int]): Seq[Int] = a
  }

  implicit object intSeqBridge2 extends /* IntLikeNum with */ Bridge[IntSeqTop, IntTop, IntSeqTop] {
    def lift1(a: Seq[Int]): Seq[Int] = a
    def lift2(a: Int     ): Seq[Int] = a :: Nil
  }

  implicit object doubleSeqBridge1 extends /* DoubleLikeNum with */ Bridge[DoubleTop, DoubleSeqTop, DoubleSeqTop] {
    def lift1(a: Double     ): Seq[Double] = a :: Nil
    def lift2(a: Seq[Double]): Seq[Double] = a
  }

  implicit object doubleSeqBridge2 extends /* DoubleLikeNum with */ Bridge[DoubleSeqTop, DoubleTop, DoubleSeqTop] {
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

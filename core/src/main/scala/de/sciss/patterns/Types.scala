/*
 *  Types.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns

import java.lang.{String => _String}

import de.sciss.numbers.{IntFunctions, DoubleFunctions => rd}
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.switch
import scala.language.higherKinds

object Types {
  type TopT[A] = Top { type Out[Tx] = A }

  object Top {
    type Seq[T <: Top] = Top { type Out[Tx] = scala.Seq[T#Out[Tx]] }

//    def Seq[T <: Top](implicit peer: T): Seq[T] = new Seq(peer)
//    final class Seq[T <: Top](val peer: T) extends Top {
//      type Out = scala.Seq[T#Out]
//    }
  }
  trait Top {
    type Out[Tx]
  }

  trait CTop extends Top {
    type Out[Tx] = COut

    type COut
  }

  object Aux {
    private val COOKIE = 0x4175   // "Au"

    def read(in: DataInput): Aux = {
      val cookie  = in.readShort()
      if (cookie != COOKIE) sys.error(s"Unexpected cookie - found ${cookie.toHexString}, expected ${COOKIE.toHexString}")
      val id      = in.readShort()
      (id: @switch) match {
        case IntTop           .id => IntTop
        case IntSeqTop        .id => IntSeqTop
        case DoubleTop        .id => DoubleTop
        case DoubleSeqTop     .id => DoubleSeqTop
//        case BooleanTop       .id => BooleanTop
//        case BooleanSeqTop    .id => BooleanSeqTop
//        case StringTop        .id => StringTop
        case Bridge.idIdentity    => Bridge.identity[Top]
        case intSeqBridge1    .id => intSeqBridge1
        case intSeqBridge2    .id => intSeqBridge2
        case doubleSeqBridge1 .id => doubleSeqBridge1
        case doubleSeqBridge2 .id => doubleSeqBridge2
        case intDoubleBridge1 .id => intDoubleBridge1
      }
    }

    def write(out: DataOutput, aux: Aux): Unit = {
      out.writeShort(COOKIE)
      out.writeShort(aux.id)
    }
  }
  sealed trait Aux {
    def id: Int
  }

  object Bridge {
    implicit def identity[A <: Top]: Bridge[A, A, A] = anyBridge.asInstanceOf[Identity[A]]

    private[Types] final val idIdentity = 0xFF

    private val anyBridge = new Identity[Top]

    private final class Identity[A <: Top] extends Bridge[A, A, A] {
      def lift1[Tx](a: A#Out[Tx]): A#Out[Tx] = a
      def lift2[Tx](a: A#Out[Tx]): A#Out[Tx] = a

      def id: Int = idIdentity
    }
  }

  trait Bridge[T1 <: Top, T2 <: Top, T <: Top] extends Aux {
    def lift1[Tx](a: T1#Out[Tx]): T#Out[Tx]
    def lift2[Tx](a: T2#Out[Tx]): T#Out[Tx]
  }

  trait Ord[T <: Top] extends Aux {
    // Ordering[T#Out]
    def lt[Tx](a: T#Out[Tx], b: T#Out[Tx]): Boolean
  }

  trait Num[T <: Top] extends Aux {
    def plus   [Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx]
    def minus  [Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx]
    def times  [Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx]
    def roundTo[Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx]
    def negate [Tx](a: T#Out[Tx]               ): T#Out[Tx]
    def abs    [Tx](a: T#Out[Tx]               ): T#Out[Tx]

    def zero[Tx]: T#Out[Tx]
    def one [Tx]: T#Out[Tx]

    def rand2[Tx](a: T#Out[Tx]              )(implicit r: Random[Tx], tx: Tx): T#Out[Tx]
    def rrand[Tx](a: T#Out[Tx], b: T#Out[Tx])(implicit r: Random[Tx], tx: Tx): T#Out[Tx]

    def fold[Tx](a: T#Out[Tx], lo: T#Out[Tx], hi: T#Out[Tx]): T#Out[Tx]
  }

  trait NumFrac[T <: Top] extends Num[T] {
    def div[Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx]
  }

  trait SeqLikeNum[A, T <: SeqTop[A]] extends Num[T] {
    type P <: CTop { type COut = A }
    protected val peer: Num[P]

    final def plus    [Tx](a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.plus    )
    final def minus   [Tx](a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.minus   )
    final def times   [Tx](a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.times   )
    final def roundTo [Tx](a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.roundTo )

    def negate[Tx](a: Seq[A]): Seq[A] = unOp(a)(peer.negate)
    def abs   [Tx](a: Seq[A]): Seq[A] = unOp(a)(peer.abs   )

    def zero[Tx]: Seq[A] = peer.zero :: Nil
    def one [Tx]: Seq[A] = peer.one  :: Nil

    def rand2[Tx](a: Seq[A]           )(implicit r: Random[Tx], tx: Tx): Seq[A] = unOp (a   )(peer.rand2[Tx])
    def rrand[Tx](a: Seq[A], b: Seq[A])(implicit r: Random[Tx], tx: Tx): Seq[A] = binOp(a, b)(peer.rrand[Tx])

    def fold[Tx](a: Seq[A], lo: Seq[A], hi: Seq[A]): Seq[A] = ternOp(a, lo, hi)(peer.fold)

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

  trait SeqLikeNumFrac[A, T <: SeqTop[A]] extends SeqLikeNum[A, T] with NumFrac[T] {
    override protected val peer: NumFrac[P]

    final def div[Tx](a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.div)
  }

  trait IntLikeNum[T <: SeqTop[Int]] extends SeqLikeNum[Int, T] {
    type P = IntTop
    protected final val peer: Num[IntTop] = IntTop
  }

  trait DoubleLikeNum[T <: SeqTop[Double]] extends SeqLikeNumFrac[Double, T] {
    type P = DoubleTop
    protected final val peer: NumFrac[DoubleTop] = DoubleTop
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

  trait ScalarOrSeqTop[A] extends CTop {
    def lift(in: COut): Seq[A]

    type Index[_]
    type COut = Index[A]

    def mapIndex[B, C](i: Index[B])(fun: B => C): Index[C]

    def traverseIndex[G[_], B, C](fa: Index[B])(f: B => G[C])(implicit app: Applicative[G]): G[Index[C]]
  }

  trait SeqTop[A] extends ScalarOrSeqTop[A] {
    type Index[B] = Seq[B]

    final def lift(in: Seq[A]): Seq[A] = in

    final def lift1[Tx](a: Seq[A]): Seq[A] = a
    final def lift2[Tx](a: Seq[A]): Seq[A] = a

    final def mapIndex[B, C](i: Index[B])(fun: B => C): Index[C] = i.map(fun)

    final def traverseIndex[G[_], B, C](fa: Index[B])(f: B => G[C])(implicit app: Applicative[G]): G[Index[C]] =
      fa.foldRight[G[Index[C]]](app.pure(Seq.empty)) { (a, lglb) =>
        app.map2(f(a), lglb)(_ +: _)
      }
  }

  trait ScalarTop[A] extends ScalarOrSeqTop[A] {
    type Index[B] = B

    final def lift(in: A): Seq[A] = in :: Nil

    final def lift1[Tx](a: A): A = a
    final def lift2[Tx](a: A): A = a

    final def mapIndex[B, C](i: Index[B])(fun: B => C): Index[C] = fun(i)

    final def traverseIndex[G[_], B, C](fa: Index[B])(f: B => G[C])(implicit app: Applicative[G]): G[Index[C]] = f(fa)
  }

  sealed trait IntLikeTop extends ScalarOrSeqTop[Int] with Top

  sealed trait IntSeqTop extends IntLikeTop with SeqTop[Int]
  implicit object IntSeqTop
    extends IntSeqTop
      with  IntLikeNum[IntSeqTop]
//      with  Bridge[IntSeqTop, IntSeqTop, IntSeqTop]
      with  Num[IntSeqTop] {

    final val id = 1
  }

  sealed trait IntTop extends IntLikeTop with ScalarTop[Int]
  implicit object IntTop
    extends IntTop
//      with  Bridge[IntTop, IntTop, IntTop]
      with  Num[IntTop]
      with  Ord[IntTop] {

    final val id = 0

    def zero   [Tx]: Int = 0
    def one    [Tx]: Int = 1

    def negate [Tx](a: Int): Int = -a
    def abs    [Tx](a: Int): Int = math.abs(a)

    def plus   [Tx](a: Int, b: Int): Int = a + b
    def minus  [Tx](a: Int, b: Int): Int = a - b
    def times  [Tx](a: Int, b: Int): Int = a * b
    def roundTo[Tx](a: Int, b: Int): Int = if (b == 0) a else math.round(a.toDouble / b).toInt * b

    def rand2[Tx](a: Int)(implicit r: Random[Tx], tx: Tx): Int = r.nextInt(2 * a + 1) - a

    def rrand[Tx](a: Int, b: Int)(implicit r: Random[Tx], tx: Tx): Int =
      r.nextInt(b - a + 1) + a

    def lt[Tx](a: Int, b: Int): Boolean = a < b

    def fold[Tx](a: Int, lo: Int, hi: Int): Int = IntFunctions.fold(a, lo, hi)
  }

  sealed trait DoubleLikeTop extends ScalarOrSeqTop[Double] with Top

  sealed trait DoubleSeqTop extends DoubleLikeTop with SeqTop[Double]
  implicit object DoubleSeqTop
    extends DoubleSeqTop
      with  DoubleLikeNum[DoubleSeqTop]
//      with  Bridge[DoubleSeqTop, DoubleSeqTop, DoubleSeqTop]
      with  Num[DoubleSeqTop] {

    final val id = 3
  }

  sealed trait DoubleTop extends DoubleLikeTop with ScalarTop[Double]
  implicit object DoubleTop
    extends DoubleTop
//      with  Bridge[DoubleTop, DoubleTop, DoubleTop]
      with  NumFrac[DoubleTop]
      with  Ord    [DoubleTop] {

    final val id = 2

    def zero   [Tx]: Double = 0.0
    def one    [Tx]: Double = 1.0

    def negate [Tx](a: Double): Double = -a
    def abs    [Tx](a: Double): Double = math.abs(a)

    def plus   [Tx](a: Double, b: Double): Double = a + b
    def minus  [Tx](a: Double, b: Double): Double = a - b
    def times  [Tx](a: Double, b: Double): Double = a * b
    def div    [Tx](a: Double, b: Double): Double = a / b
    def roundTo[Tx](a: Double, b: Double): Double = rd.roundTo(a, b)

    def rand2[Tx](a: Double)(implicit r: Random[Tx], tx: Tx): Double =
      (r.nextDouble() * 2 - 1) * a

    def rrand[Tx](a: Double, b: Double)(implicit r: Random[Tx], tx: Tx): Double =
      r.nextDouble() * (b - a) + a

    def lt[Tx](a: Double, b: Double): Boolean = a < b

    def fold[Tx](a: Double, lo: Double, hi: Double): Double = rd.fold(a, lo, hi)
  }

  sealed trait BooleanLikeTop extends ScalarOrSeqTop[Boolean] with Top

  sealed trait BooleanSeqTop extends BooleanLikeTop with SeqTop[Boolean]
  implicit object BooleanSeqTop
    extends BooleanSeqTop
//      with  Bridge[IntSeqTop, IntSeqTop, IntSeqTop]
      {

    final val id = 5
  }

  sealed trait BooleanTop extends BooleanLikeTop with ScalarTop[Boolean]
  implicit object BooleanTop
    extends BooleanTop
      //      with  Bridge[IntTop, IntTop, IntTop]
  {

    final val id = 4
  }

  implicit object intSeqBridge1 extends /* IntLikeNum with */ Bridge[IntTop, IntSeqTop, IntSeqTop] {
    def lift1[Tx](a: Int     ): Seq[Int] = a :: Nil
    def lift2[Tx](a: Seq[Int]): Seq[Int] = a

    final val id = 0x100
  }

  implicit object intSeqBridge2 extends /* IntLikeNum with */ Bridge[IntSeqTop, IntTop, IntSeqTop] {
    def lift1[Tx](a: Seq[Int]): Seq[Int] = a
    def lift2[Tx](a: Int     ): Seq[Int] = a :: Nil

    final val id = 0x101
  }

  implicit object doubleSeqBridge1 extends /* DoubleLikeNum with */ Bridge[DoubleTop, DoubleSeqTop, DoubleSeqTop] {
    def lift1[Tx](a: Double     ): Seq[Double] = a :: Nil
    def lift2[Tx](a: Seq[Double]): Seq[Double] = a

    final val id = 0x102
  }

  implicit object doubleSeqBridge2 extends /* DoubleLikeNum with */ Bridge[DoubleSeqTop, DoubleTop, DoubleSeqTop] {
    def lift1[Tx](a: Seq[Double]): Seq[Double] = a
    def lift2[Tx](a: Double     ): Seq[Double] = a :: Nil

    final val id = 0x103
  }

  implicit object intDoubleBridge1 extends Bridge[IntTop, DoubleTop, DoubleTop] {
    def lift1[Tx](a: Int    ): Double = a.toDouble
    def lift2[Tx](a: Double ): Double = a

    final val id = 0x104
  }

  ////////////////////////////

  sealed trait StringTop extends CTop {
    type COut = _String
  }
  implicit object StringTop extends StringTop
//    with Bridge[StringTop, StringTop, StringTop]
  {
    def lift1[Tx](a: _String): _String = a
    def lift2[Tx](a: _String): _String = a

    final val id = 10
  }
}
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

import de.sciss.numbers.{DoubleFunctions => rd, IntFunctions => ri}
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.switch

object Types {
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
        case BooleanTop       .id => BooleanTop
//        case BooleanSeqTop    .id => BooleanSeqTop
//        case StringTop        .id => StringTop
        case Widen.idIdentity    => Widen.identity[Any]
        case intSeqWiden1     .id => intSeqWiden1
        case intSeqWiden2     .id => intSeqWiden2
        case doubleSeqWiden1  .id => doubleSeqWiden1
        case doubleSeqWiden2  .id => doubleSeqWiden2
        case intDoubleWiden1  .id => intDoubleWiden1
        case intDoubleWiden2  .id => intDoubleWiden2
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

  object Widen {
    implicit def identity[A]: Widen[A, A, A] = anyWiden.asInstanceOf[Identity[A]]

    private[Types] final val idIdentity = 0xFF

    private val anyWiden = new Identity[Any]

    private final class Identity[A] extends Widen[A, A, A] {
      def lift1(a: A): A = a
      def lift2(a: A): A = a

      def id: Int = idIdentity
    }
  }

  trait Widen[A1, A2, A] extends Aux {
    def lift1(a: A1): A
    def lift2(a: A2): A
  }

  trait Ord[A] extends Aux {
    // Ordering[T#Out]
    def lt (a: A, b: A): Boolean
    def leq(a: A, b: A): Boolean
    def gt (a: A, b: A): Boolean
    def geq(a: A, b: A): Boolean
  }

  trait Num[A] extends Aux {
    // binary
    def plus   (a: A, b: A): A
    def minus  (a: A, b: A): A
    def times  (a: A, b: A): A
    def roundTo(a: A, b: A): A
    def %      (a: A, b: A): A
    def mod    (a: A, b: A): A

    // unary
    def negate (a: A): A
    def abs    (a: A): A

    def zero: A
    def one : A

//    def zeroPat: Pat[A]
//    def onePat : Pat[A]

    def rand2[Tx](a: A      )(implicit r: Random[Tx], tx: Tx): A
    def rrand[Tx](a: A, b: A)(implicit r: Random[Tx], tx: Tx): A

    def fold (a: A, lo: A, hi: A): A
  }

  trait NumFrac[A] extends Num[A] {
    def div(a: A, b: A): A
  }

//  trait NumIntegral[T <: Top] extends Num[T] {
//    def %   [Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx]
//    def quot[Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx]
//    def rem [Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx]
//  }

  trait ToNum[A] extends Aux {
    def toInt   (a: A): Int
    def toDouble(a: A): Double
  }

  trait SeqLikeNum[A] extends Num[Seq[A]] {
//    type P // <: CTop { type COut = A }
    protected val peer: Num[A] // Num[P]

    final def plus    (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.plus    )
    final def minus   (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.minus   )
    final def times   (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.times   )
    final def roundTo (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.roundTo )
    final def %       (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.%       )
    final def mod     (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.mod     )

    def negate (a: Seq[A]): Seq[A] = unOp(a)(peer.negate)
    def abs    (a: Seq[A]): Seq[A] = unOp(a)(peer.abs   )

    def zero : Seq[A] = peer.zero :: Nil
    def one  : Seq[A] = peer.one  :: Nil

//    def zeroPat : Pat[Seq[A]] = ... // Pat[Top.CSeq[A]](peer.zero :: Nil)
//    def onePat  : Pat[Seq[A]] = ... // Pat[Top.CSeq[A]](peer.one  :: Nil)

    def rand2[Tx](a: Seq[A]           )(implicit r: Random[Tx], tx: Tx): Seq[A] = unOp (a   )(peer.rand2[Tx])
    def rrand[Tx](a: Seq[A], b: Seq[A])(implicit r: Random[Tx], tx: Tx): Seq[A] = binOp(a, b)(peer.rrand[Tx])

    def fold(a: Seq[A], lo: Seq[A], hi: Seq[A]): Seq[A] = ternOp(a, lo, hi)(peer.fold)

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

  trait SeqLikeNumFrac[A] extends SeqLikeNum[A] with NumFrac[Seq[A]] {
    override protected val peer: NumFrac[A]

    final def div(a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.div)
  }

  trait IntLikeNum extends SeqLikeNum[Int] {
    protected final val peer: Num[Int] = IntTop
  }

  trait DoubleLikeNum extends SeqLikeNumFrac[Double] {
//    type P = DoubleTop
    protected final val peer: NumFrac[Double] = DoubleTop
  }

//  object Applicative {
//    implicit object Iterator extends Applicative[scala.Iterator] {
//      def pure[A](x: A): scala.Iterator[A] = scala.Iterator.single(x)
//
//      def map[A, B](fa: Iterator[A])(f: A => B): Iterator[B] = fa.map(f)
//
//      def flatMap[A, B](fa: Iterator[A])(f: A => Iterator[B]): Iterator[B] =
//        fa.flatMap(f)
//    }
//  }
//  // XXX TODO -- a bit ad-hoc; we could just depend on Cats
//  trait Applicative[F[_]] {
//    def pure[A](x: A): F[A]
//
//    def map    [A, B](fa: F[A])(f: A =>   B ): F[B]
//    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
//
//    def ap     [A, B   ](ff: F[A => B])(fa: F[A]): F[B]             = flatMap(ff)(f => map(fa)(f))
//    def product[A, B   ](fa: F[A], fb: F[B]): F[(A, B)]             = ap(map(fa)(a => (b: B) => (a, b)))(fb)
//    def map2   [A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z]  = map(product(fa, fb)) { case (a, b) => f(a, b) }
//  }

//  trait ScalarOrSeqTop[A] { // extends CTop {
//    def lift(in: COut): Seq[A]

//    type Index[_]
//    type COut = Index[A]
//
//    def mapIndex[B, C](i: Index[B])(fun: B => C): Index[C]
//
//    def traverseIndex[G[_], B, C](fa: Index[B])(f: B => G[C])(implicit app: Applicative[G]): G[Index[C]]
//  }

//  trait SeqTop[A] extends ScalarOrSeqTop[A] {
//    type Index[B] = Seq[B]

//    final def lift(in: Seq[A]): Seq[A] = in

//    final def lift1[Tx](a: Seq[A]): Seq[A] = a
//    final def lift2[Tx](a: Seq[A]): Seq[A] = a

//    final def mapIndex[B, C](i: Index[B])(fun: B => C): Index[C] = i.map(fun)
//
//    final def traverseIndex[G[_], B, C](fa: Index[B])(f: B => G[C])(implicit app: Applicative[G]): G[Index[C]] =
//      fa.foldRight[G[Index[C]]](app.pure(Seq.empty)) { (a, lglb) =>
//        app.map2(f(a), lglb)(_ +: _)
//      }
//  }

//  trait ScalarTop[A] extends ScalarOrSeqTop[A] {
//    type Index[B] = B

//    final def lift(in: A): Seq[A] = in :: Nil

//    final def lift1[Tx](a: A): A = a
//    final def lift2[Tx](a: A): A = a

//    final def mapIndex[B, C](i: Index[B])(fun: B => C): Index[C] = fun(i)
//
//    final def traverseIndex[G[_], B, C](fa: Index[B])(f: B => G[C])(implicit app: Applicative[G]): G[Index[C]] = f(fa)
//  }

//  sealed trait IntLikeTop extends ScalarOrSeqTop[Int] // with Top

//  sealed trait IntSeqTop extends IntLikeTop with SeqTop[Int]
  implicit object IntSeqTop
//    extends IntSeqTop
      extends IntLikeNum
      with  Num[Seq[Int]] {

    final val id = 1

//    private[patterns] val cClassTag: ClassTag[COut] = ClassTag(classOf[COut])
  }

//  sealed trait IntTop extends IntLikeTop with ScalarTop[Int]
  implicit object IntTop
//    extends IntTop
      extends Num /* NumIntegral */[Int]
      with    ToNum[Int]
      with    Ord[Int] {

    final val id = 0

    def zero   : Int = 0
    def one    : Int = 1

//    def zeroPat: Pat[Int] = 0
//    def onePat : Pat[Int] = 1

    def negate (a: Int): Int = -a
    def abs    (a: Int): Int = math.abs(a)

    def toInt   (a: Int): Int     = a
    def toDouble(a: Int): Double  = a.toDouble

    def plus   (a: Int, b: Int): Int = a + b
    def minus  (a: Int, b: Int): Int = a - b
    def times  (a: Int, b: Int): Int = a * b
    def roundTo(a: Int, b: Int): Int = if (b == 0) a else math.round(a.toDouble / b).toInt * b

    def %      (a: Int, b: Int): Int = a % b
    def mod    (a: Int, b: Int): Int = ri.mod(a, b)
//    def quot   [Tx](a: Int, b: Int): Int = a / b

    def rand2[Tx](a: Int)(implicit r: Random[Tx], tx: Tx): Int = r.nextInt(2 * a + 1) - a

    def rrand[Tx](a: Int, b: Int)(implicit r: Random[Tx], tx: Tx): Int =
      r.nextInt(b - a + 1) + a

    def lt  (a: Int, b: Int): Boolean = a <  b
    def leq (a: Int, b: Int): Boolean = a <= b
    def gt  (a: Int, b: Int): Boolean = a >  b
    def geq (a: Int, b: Int): Boolean = a >= b

    def fold (a: Int, lo: Int, hi: Int): Int = ri.fold(a, lo, hi)

//    private[patterns] val cClassTag: ClassTag[COut] = ClassTag.Int
  }

//  sealed trait DoubleLikeTop extends ScalarOrSeqTop[Double] // with Top

//  sealed trait DoubleSeqTop extends DoubleLikeTop with SeqTop[Double]
  implicit object DoubleSeqTop
//    extends DoubleSeqTop
      extends DoubleLikeNum
      with  Num[Seq[Double]] {

    final val id = 3

//    private[patterns] val cClassTag: ClassTag[COut] = ClassTag(classOf[COut])
  }

//  sealed trait DoubleTop extends DoubleLikeTop with ScalarTop[Double]
  implicit object DoubleTop
//    extends DoubleTop
      extends NumFrac[Double]
      with    ToNum  [Double]
      with    Ord    [Double] {

    final val id = 2

    def zero   : Double = 0.0
    def one    : Double = 1.0

//    def zeroPat: Pat[Double] = 0.0
//    def onePat : Pat[Double] = 1.0

    def negate (a: Double): Double = -a
    def abs    (a: Double): Double = math.abs(a)

    def toInt   (a: Double): Int     = a.toInt
    def toDouble(a: Double): Double  = a

    def plus   (a: Double, b: Double): Double = a + b
    def minus  (a: Double, b: Double): Double = a - b
    def times  (a: Double, b: Double): Double = a * b
    def div    (a: Double, b: Double): Double = a / b
    def roundTo(a: Double, b: Double): Double = rd.roundTo(a, b)

    def %      (a: Double, b: Double): Double = a % b
    def mod    (a: Double, b: Double): Double = rd.mod(a, b)

    def rand2[Tx](a: Double)(implicit r: Random[Tx], tx: Tx): Double =
      (r.nextDouble() * 2 - 1) * a

    def rrand[Tx](a: Double, b: Double)(implicit r: Random[Tx], tx: Tx): Double =
      r.nextDouble() * (b - a) + a

    def lt (a: Double, b: Double): Boolean = a <  b
    def leq(a: Double, b: Double): Boolean = a <= b
    def gt (a: Double, b: Double): Boolean = a >  b
    def geq(a: Double, b: Double): Boolean = a >= b

    def fold(a: Double, lo: Double, hi: Double): Double = rd.fold(a, lo, hi)

//    private[patterns] val cClassTag: ClassTag[COut] = ClassTag.Double
  }

//  sealed trait BooleanLikeTop extends ScalarOrSeqTop[Boolean] // with Top

//  sealed trait BooleanSeqTop extends BooleanLikeTop with SeqTop[Boolean]
//  implicit object BooleanSeqTop extends BooleanSeqTop {
//
//    final val id = 5
//
////    private[patterns] val cClassTag: ClassTag[COut] = ClassTag(classOf[COut])
//  }

//  sealed trait BooleanTop extends BooleanLikeTop with ScalarTop[Boolean]
  implicit object BooleanTop
    extends ToNum[Boolean] {

    final val id = 4

    def toInt   (a: Boolean): Int     = if (a) 1   else 0
    def toDouble(a: Boolean): Double  = if (a) 1.0 else 0.0

  //    private[patterns] val cClassTag: ClassTag[COut] = ClassTag.Boolean
  }

  implicit object intSeqWiden1 extends /* IntLikeNum with */ Widen[Int, Seq[Int], Seq[Int]] {
    def lift1(a: Int     ): Seq[Int] = a :: Nil
    def lift2(a: Seq[Int]): Seq[Int] = a

    final val id = 0x100
  }

  implicit object intSeqWiden2 extends /* IntLikeNum with */ Widen[Seq[Int], Int, Seq[Int]] {
    def lift1(a: Seq[Int]): Seq[Int] = a
    def lift2(a: Int     ): Seq[Int] = a :: Nil

    final val id = 0x101
  }

  implicit object doubleSeqWiden1 extends /* DoubleLikeNum with */ Widen[Double, Seq[Double], Seq[Double]] {
    def lift1(a: Double     ): Seq[Double] = a :: Nil
    def lift2(a: Seq[Double]): Seq[Double] = a

    final val id = 0x102
  }

  implicit object doubleSeqWiden2 extends /* DoubleLikeNum with */ Widen[Seq[Double], Double, Seq[Double]] {
    def lift1(a: Seq[Double]): Seq[Double] = a
    def lift2(a: Double     ): Seq[Double] = a :: Nil

    final val id = 0x103
  }

  implicit object intDoubleWiden1 extends Widen[Int, Double, Double] {
    def lift1(a: Int    ): Double = a.toDouble
    def lift2(a: Double ): Double = a

    final val id = 0x104
  }

  implicit object intDoubleWiden2 extends Widen[Double, Int, Double] {
    def lift1(a: Double ): Double = a
    def lift2(a: Int    ): Double = a.toDouble

    final val id = 0x105
  }
  ////////////////////////////

//  sealed trait StringTop extends CTop {
//    type COut = _String
//  }
//
//  implicit object StringTop extends StringTop {
//    def lift1[Tx](a: _String): _String = a
//    def lift2[Tx](a: _String): _String = a
//
//    final val id = 10
//
////    private[patterns] val cClassTag: ClassTag[COut] = ClassTag(classOf[COut])
//  }

//  trait Tuple2Top[T1 <: Top, T2 <: Top] extends Top {
//    type Out[Tx] = (T1#Out[Tx], T2#Out[Tx])
//  }
}
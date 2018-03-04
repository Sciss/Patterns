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

import de.sciss.numbers.{DoubleFunctions => rd, IntFunctions => ri, IntFunctions2 => ri2}
import de.sciss.patterns.impl.{ScalarEqImpl, ScalarToNumImpl, SeqLikeEq, SeqLikeNum, SeqLikeNumDouble, SeqLikeNumFrac, SeqLikeToNum}
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
        case IntTop                 .id => IntTop
        case IntSeqTop              .id => IntSeqTop
        case DoubleTop              .id => DoubleTop
        case DoubleSeqTop           .id => DoubleSeqTop
        case BooleanTop             .id => BooleanTop
        case BooleanSeqTop          .id => BooleanSeqTop
        case StringTop              .id => StringTop
        case Widen2.idIdentity          => Widen2.identity[Any]
        case Widen2.intSeqSeq       .id => Widen2.intSeqSeq
        case Widen2.seqIntSeq       .id => Widen2.seqIntSeq
        case Widen2.doubleSeqSeq    .id => Widen2.doubleSeqSeq
        case Widen2.seqDoubleSeq    .id => Widen2.seqDoubleSeq
        case Widen2.intDoubleDouble .id => Widen2.intDoubleDouble
        case Widen2.doubleIntDouble .id => Widen2.doubleIntDouble
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
    implicit def identity[A]: Widen[A, A] = Widen2.identity[A]
  }

  trait Widen[A1, A] extends Aux {
    def widen1(a: A1): A
  }

  object Widen2 {
    implicit def identity[A]: Widen2[A, A, A] = anyWiden.asInstanceOf[Identity[A]]

    implicit object intSeqSeq extends Widen2[Int, Seq[Int], Seq[Int]] {
      def widen1(a: Int     ): Seq[Int] = a :: Nil
      def widen2(a: Seq[Int]): Seq[Int] = a

      final val id = 0x100
    }

    implicit object seqIntSeq extends Widen2[Seq[Int], Int, Seq[Int]] {
      def widen1(a: Seq[Int]): Seq[Int] = a
      def widen2(a: Int     ): Seq[Int] = a :: Nil

      final val id = 0x101
    }

    implicit object doubleSeqSeq extends Widen2[Double, Seq[Double], Seq[Double]] {
      def widen1(a: Double     ): Seq[Double] = a :: Nil
      def widen2(a: Seq[Double]): Seq[Double] = a

      final val id = 0x102
    }

    implicit object seqDoubleSeq extends Widen2[Seq[Double], Double, Seq[Double]] {
      def widen1(a: Seq[Double]): Seq[Double] = a
      def widen2(a: Double     ): Seq[Double] = a :: Nil

      final val id = 0x103
    }

    implicit object intDoubleDouble extends Widen2[Int, Double, Double] {
      def widen1(a: Int    ): Double = a.toDouble
      def widen2(a: Double ): Double = a

      final val id = 0x104
    }

    implicit object doubleIntDouble extends Widen2[Double, Int, Double] {
      def widen1(a: Double ): Double = a
      def widen2(a: Int    ): Double = a.toDouble

      final val id = 0x105
    }

    private[Types] final val idIdentity = 0xFF

    private val anyWiden = new Identity[Any]

    private final class Identity[A] extends Widen2[A, A, A] {
      def widen1(a: A): A = a
      def widen2(a: A): A = a

      def id: Int = idIdentity
    }
  }

  trait Widen2[A1, A2, A] extends Widen[A1, A] {
    def widen1(a: A1): A
    def widen2(a: A2): A
  }

  trait Eq[A] extends Aux {
    type Boolean

    def eq (a: A, b: A): Boolean
    def neq(a: A, b: A): Boolean
  }

  trait Ord[A] extends Eq[A] {
    def lt (a: A, b: A): Boolean
    def leq(a: A, b: A): Boolean
    def gt (a: A, b: A): Boolean
    def geq(a: A, b: A): Boolean
  }

  type ScalarOrd[A] = Ord[A] with Scalar[A]

  trait Num[A] extends Ord[A] {
    // binary
    def +         (a: A, b: A): A
    def -         (a: A, b: A): A
    def *         (a: A, b: A): A
    def %         (a: A, b: A): A
    def mod       (a: A, b: A): A
    def min       (a: A, b: A): A
    def max       (a: A, b: A): A
    def roundTo   (a: A, b: A): A
    def roundUpTo (a: A, b: A): A
    def trunc     (a: A, b: A): A
    //    def ring1     (a: A, b: A): A
    //    def ring2     (a: A, b: A): A
    //    def ring3     (a: A, b: A): A
    //    def ring4     (a: A, b: A): A
    def difsqr    (a: A, b: A): A
    def sumsqr    (a: A, b: A): A
    def sqrsum    (a: A, b: A): A
    def sqrdif    (a: A, b: A): A
    def absdif    (a: A, b: A): A
    //    def thresh    (a: A, b: A): A
    //    def amclip    (a: A, b: A): A
    //    def scaleneg  (a: A, b: A): A
    def clip2     (a: A, b: A): A
    def excess    (a: A, b: A): A
    def fold2     (a: A, b: A): A
    def wrap2     (a: A, b: A): A

    // unary
    def negate (a: A): A
    def abs    (a: A): A
    def signum (a: A): A

    def squared   (a: A): A
    def cubed     (a: A): A

    def zero: A
    def one : A

    // random
    def rand [Tx](a: A      )(implicit r: Random[Tx], tx: Tx): A
    def rand2[Tx](a: A      )(implicit r: Random[Tx], tx: Tx): A
    def rrand[Tx](a: A, b: A)(implicit r: Random[Tx], tx: Tx): A

    // ternary
    def fold (a: A, lo: A, hi: A): A
  }

  type ScalarNum[A] = Num[A] with Scalar[A]

  trait NumFrac[A] extends Num[A] {
    def floor (a: A): A
    def ceil  (a: A): A
    def frac  (a: A): A

    def /     (a: A, b: A): A

    def reciprocal(a: A): A
  }

  type ScalarNumFrac[A] = NumFrac[A] with Scalar[A]

  trait NumBool[A] extends Eq[A] {
    def not(a: A): A
  }

  type ScalarNumBool[A] = NumBool[A] with Scalar[A]

  trait NumInt[A] extends Num[A] {
    def unary_~ (a: A): A

    def &       (a: A, b: A): A
    def |       (a: A, b: A): A
    def ^       (a: A, b: A): A

    def lcm     (a: A, b: A): A
    def gcd     (a: A, b: A): A

    def <<      (a: A, b: A): A
    def >>      (a: A, b: A): A
    def >>>     (a: A, b: A): A
  }

  type ScalarNumInt[A] = NumInt[A] with Scalar[A]

  trait NumDouble[A] extends NumFrac[A] {
    def sqrt      (a: A): A
    def exp       (a: A): A
    def midicps   (a: A): A
    def cpsmidi   (a: A): A
    def midiratio (a: A): A
    def ratiomidi (a: A): A
    def dbamp     (a: A): A
    def ampdb     (a: A): A
    def octcps    (a: A): A
    def cpsoct    (a: A): A
    def log       (a: A): A
    def log2      (a: A): A
    def log10     (a: A): A
    def sin       (a: A): A
    def cos       (a: A): A
    def tan       (a: A): A
    def asin      (a: A): A
    def acos      (a: A): A
    def atan      (a: A): A
    def sinh      (a: A): A
    def cosh      (a: A): A
    def tanh      (a: A): A

    def atan2     (a: A, b: A): A
    def hypot     (a: A, b: A): A
    def hypotx    (a: A, b: A): A
    def pow       (a: A, b: A): A

    def coin[Tx](a: A)(implicit r: Random[Tx], tx: Tx): Boolean
  }

  type ScalarNumDouble[A] = NumDouble[A] with Scalar[A]

  trait ToNum[A] extends Aux {
    type Int
    type Double

    def toInt   (a: A): Int
    def toDouble(a: A): Double

    def int   : NumInt   [Int]
    def double: NumDouble[Double]
  }

  type ScalarToNum[A] = ToNum[A] with Scalar[A]

  trait Scalar[A] {
    final type In       = A

    final type Boolean  = scala.Boolean
    final type Int      = scala.Int
    final type Double   = scala.Double
  }

  type ScalarEq[A] = Eq[A] with Scalar[A]

  implicit final object IntSeqTop
    extends NumInt      [Seq[Int]]
    with    SeqLikeNum  [Int]
    with    SeqLikeToNum[Int] {

    protected val peer: IntTop.type = IntTop

    def unary_~(a: In): In = unOp(a)(peer.unary_~)

    def &   (a: In, b: In): In = binOp(a, b)(peer.&)
    def |   (a: In, b: In): In = binOp(a, b)(peer.|)
    def ^   (a: In, b: In): In = binOp(a, b)(peer.^)

    def lcm (a: In, b: In): In = binOp(a, b)(peer.lcm)
    def gcd (a: In, b: In): In = binOp(a, b)(peer.gcd)

    def <<  (a: In, b: In): In = binOp(a, b)(peer.<<)
    def >>  (a: In, b: In): In = binOp(a, b)(peer.>>)
    def >>> (a: In, b: In): In = binOp(a, b)(peer.>>>)

    final val id = 1
  }

  implicit final object IntTop
    extends NumInt          [Int]
    with    ScalarEqImpl    [Int]
    with    ScalarToNumImpl [Int] {

    final val id = 0

    def zero   : Int = 0
    def one    : Int = 1

    def toInt     (a: Int): Int     = a
    def toDouble  (a: Int): Double  = a.toDouble

    def +(a: Int, b: Int): Int = a + b
    def -(a: Int, b: Int): Int = a - b
    def *(a: Int, b: Int): Int = a * b
    def %         (a: Int, b: Int): Int = a % b
    def mod       (a: Int, b: Int): Int = ri.mod(a, b)
    def min       (a: Int, b: Int): Int = ri.min(a, b)
    def max       (a: Int, b: Int): Int = ri.max(a, b)

    def &         (a: Int, b: Int): Int = a & b
    def |         (a: Int, b: Int): Int = a | b
    def ^         (a: Int, b: Int): Int = a ^ b
    def lcm       (a: Int, b: Int): Int = ri.lcm(a, b)
    def gcd       (a: Int, b: Int): Int = ri.gcd(a, b)

    def roundTo   (a: Int, b: Int): Int = ri2.roundTo  (a, b)
    def roundUpTo (a: Int, b: Int): Int = ri2.roundUpTo(a, b)
    def trunc     (a: Int, b: Int): Int = ri2.trunc    (a, b)

    def <<        (a: Int, b: Int): Int = a << b
    def >>        (a: Int, b: Int): Int = a >> b
    def >>>       (a: Int, b: Int): Int = a >>> b

    def difsqr    (a: Int, b: Int): Int = ri2.difsqr(a, b).toInt
    def sumsqr    (a: Int, b: Int): Int = ri2.sumsqr(a, b).toInt
    def sqrsum    (a: Int, b: Int): Int = ri2.sqrsum(a, b).toInt
    def sqrdif    (a: Int, b: Int): Int = ri2.sqrdif(a, b).toInt
    def absdif    (a: Int, b: Int): Int = ri2.absdif(a, b)

    def clip2     (a: Int, b: Int): Int = ri.clip2  (a, b)
    def excess    (a: Int, b: Int): Int = ri.excess (a, b)
    def fold2     (a: Int, b: Int): Int = ri.fold2  (a, b)
    def wrap2     (a: Int, b: Int): Int = ri.wrap2  (a, b)

    def negate    (a: Int): Int     = -a
    def abs       (a: Int): Int     = ri.abs(a)
    def signum    (a: Int): Int     = ri.signum(a)

    def unary_~   (a: Int): Int = ~a

    def squared   (a: Int): Int = ri.squared(a).toInt
    def cubed     (a: Int): Int = ri2.cubed (a).toInt

    def rand[Tx](a: Int)(implicit r: Random[Tx], tx: Tx): Int =
      if (a >= 0) r.nextInt( a)     // may throw exception
      else        r.nextInt(-a) + a

    def rand2[Tx](a: Int)(implicit r: Random[Tx], tx: Tx): Int = {
      val a1 = math.abs(a)
      r.nextInt(2 * a1 + 1) - a1
    }

    def rrand[Tx](a: Int, b: Int)(implicit r: Random[Tx], tx: Tx): Int =
      if (a < b) r.nextInt(b - a + 1) + a
      else       r.nextInt(a - b + 1) + b

    def lt  (a: Int, b: Int): Boolean = a <  b
    def leq (a: Int, b: Int): Boolean = a <= b
    def gt  (a: Int, b: Int): Boolean = a >  b
    def geq (a: Int, b: Int): Boolean = a >= b

    def fold (a: Int, lo: Int, hi: Int): Int = ri.fold(a, lo, hi)
  }

  implicit final object DoubleSeqTop
    extends SeqLikeNumFrac  [Double]
      with  SeqLikeToNum    [Double]
      with  SeqLikeNumDouble[Double] {

    protected final val peer: DoubleTop.type = DoubleTop

    final val id = 3
  }

  implicit final object DoubleTop
    extends NumDouble       [Double]
    with    ScalarEqImpl    [Double]
    with    ScalarToNumImpl [Double] {

    final val id = 2

    def zero   : In = 0.0
    def one    : In = 1.0

    // binary

    def +(a: In, b: In): In = rd.+(a, b)
    def -(a: In, b: In): In = rd.-(a, b)
    def *(a: In, b: In): In = rd.*(a, b)
    def /         (a: In, b: In): In = rd./(a, b)
    def %         (a: In, b: In): In = rd.%  (a, b)
    def mod       (a: In, b: In): In = rd.mod(a, b)

    def lt        (a: In, b: In): Boolean = a <  b
    def leq       (a: In, b: In): Boolean = a <= b
    def gt        (a: In, b: In): Boolean = a >  b
    def geq       (a: In, b: In): Boolean = a >= b

    def min       (a: In, b: In): In = rd.min(a, b)
    def max       (a: In, b: In): In = rd.max(a, b)

    def roundTo   (a: In, b: In): In = rd.roundTo(a, b)
    def roundUpTo (a: In, b: In): In = rd.roundUpTo(a, b)
    def trunc     (a: In, b: In): In = rd.trunc(a, b)

    def atan2     (a: In, b: In): In = rd.atan2 (a, b)
    def hypot     (a: In, b: In): In = rd.hypot (a, b)
    def hypotx    (a: In, b: In): In = rd.hypotx(a, b)
    def pow       (a: In, b: In): In = rd.pow   (a, b)

    def difsqr    (a: In, b: In): In = rd.difsqr(a, b)
    def sumsqr    (a: In, b: In): In = rd.sumsqr(a, b)
    def sqrsum    (a: In, b: In): In = rd.sqrsum(a, b)
    def sqrdif    (a: In, b: In): In = rd.sqrdif(a, b)
    def absdif    (a: In, b: In): In = rd.absdif(a, b)

    def clip2     (a: In, b: In): In = rd.clip2(a, b)
    def excess    (a: In, b: In): In = rd.excess(a, b)
    def fold2     (a: In, b: In): In = rd.fold2(a, b)
    def wrap2     (a: In, b: In): In = rd.wrap2(a, b)

    // unary

    def negate    (a: In): In = -a
    def abs       (a: In): In = rd.abs(a)
    def signum    (a: In): In = rd.signum(a)

    def toInt     (a: In): Int    = a.toInt
    def toDouble  (a: In): Double = a

    def floor     (a: In): In     = rd.floor    (a)
    def ceil      (a: In): In     = rd.ceil     (a)
    def frac      (a: In): In     = rd.frac     (a)
    def midicps   (a: In): In     = rd.midicps  (a)
    def cpsmidi   (a: In): In     = rd.cpsmidi  (a)
    def midiratio (a: In): In     = rd.midiratio(a)
    def ratiomidi (a: In): In     = rd.ratiomidi(a)
    def dbamp     (a: In): In     = rd.dbamp    (a)
    def ampdb     (a: In): In     = rd.ampdb    (a)
    def octcps    (a: In): In     = rd.octcps   (a)
    def cpsoct    (a: In): In     = rd.cpsoct   (a)
    def log       (a: In): In     = rd.log      (a)
    def log2      (a: In): In     = rd.log2     (a)
    def log10     (a: In): In     = rd.log10    (a)
    def sin       (a: In): In     = rd.sin      (a)
    def cos       (a: In): In     = rd.cos      (a)
    def tan       (a: In): In     = rd.tan      (a)
    def asin      (a: In): In     = rd.asin     (a)
    def acos      (a: In): In     = rd.acos     (a)
    def atan      (a: In): In     = rd.atan     (a)
    def sinh      (a: In): In     = rd.sinh     (a)
    def cosh      (a: In): In     = rd.cosh     (a)
    def tanh      (a: In): In     = rd.tanh     (a)

    def sqrt(a: In): In = rd.sqrt(a)
    def exp (a: In): In = rd.exp (a)

    def squared (a: In): In = rd.squared(a)
    def cubed   (a: In): In = a * a * a

    def reciprocal(a: In): In = 1.0 / a

    def rand[Tx](a: In)(implicit r: Random[Tx], tx: Tx): In =
      r.nextDouble() * a

    def rand2[Tx](a: In)(implicit r: Random[Tx], tx: Tx): In =
      (r.nextDouble() * 2 - 1) * a

    def rrand[Tx](a: In, b: In)(implicit r: Random[Tx], tx: Tx): In =
      r.nextDouble() * (b - a) + a

    def coin[Tx](a: In)(implicit r: Random[Tx], tx: Tx): Boolean =
      r.nextDouble() < a

    def fold(a: In, lo: In, hi: In): In = rd.fold(a, lo, hi)
  }

  implicit final object BooleanSeqTop
    extends NumBool[Seq[Boolean]]
    with SeqLikeEq    [Boolean]
    with SeqLikeToNum [Boolean] {

    final val peer: BooleanTop.type = BooleanTop

    final val id = 5

    def not(a: In): In = unOp(a)(!_)
  }

  implicit final object BooleanTop
    extends NumBool     [Boolean]
    with ScalarEqImpl   [Boolean]
    with ScalarToNumImpl[Boolean] {

    final val id = 4

    def toInt   (a: In): Int     = if (a) 1   else 0
    def toDouble(a: In): Double  = if (a) 1.0 else 0.0

    def not(a: In): In = !a
  }

  implicit final object StringTop
    extends ScalarEqImpl[String] {

    final val id = 10
  }
}
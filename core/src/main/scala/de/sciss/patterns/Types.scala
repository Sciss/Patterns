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
import scala.language.higherKinds

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
        case BooleanSeqTop    .id => BooleanSeqTop
        case StringTop        .id => StringTop
        case Widen2.idIdentity    => Widen2.identity[Any]
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
    implicit def identity[A]: Widen[A, A] = Widen2.identity[A]
  }

  trait Widen[A1, A] extends Aux {
    def widen1(a: A1): A
  }

  object Widen2 {
    implicit def identity[A]: Widen2[A, A, A] = anyWiden.asInstanceOf[Identity[A]]

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

  trait EqC[C[_], A] extends Aux {
    def eq (a: A, b: A): C[Boolean]
    def neq(a: A, b: A): C[Boolean]
  }

  trait Eq[A] extends EqC[Id, A]

  trait OrdC[C[_], A] extends EqC[C, A] {
    def lt (a: A, b: A): C[Boolean]
    def leq(a: A, b: A): C[Boolean]
    def gt (a: A, b: A): C[Boolean]
    def geq(a: A, b: A): C[Boolean]
  }

  trait Ord[A] extends OrdC[Id, A] with Eq[A]

  trait Num[A] extends Aux {
    // binary
    def plus      (a: A, b: A): A
    def minus     (a: A, b: A): A
    def times     (a: A, b: A): A
    def roundTo   (a: A, b: A): A
    def roundUpTo (a: A, b: A): A
    def trunc     (a: A, b: A): A
    def %         (a: A, b: A): A
    def mod       (a: A, b: A): A
    def min       (a: A, b: A): A
    def max       (a: A, b: A): A
    def clip2     (a: A, b: A): A
    def fold2     (a: A, b: A): A
    def wrap2     (a: A, b: A): A

    def squared   (a: A): A
    def cubed     (a: A): A

    // unary
    def negate (a: A): A
    def abs    (a: A): A
    def signum (a: A): A

    def zero: A
    def one : A

    def rand [Tx](a: A      )(implicit r: Random[Tx], tx: Tx): A
    def rand2[Tx](a: A      )(implicit r: Random[Tx], tx: Tx): A
    def rrand[Tx](a: A, b: A)(implicit r: Random[Tx], tx: Tx): A

    def fold (a: A, lo: A, hi: A): A
  }

  trait NumFrac[ A] extends Num[A] {
    def floor (a: A): A
    def ceil  (a: A): A
    def frac  (a: A): A

    def div(a: A, b: A): A

    def reciprocal(a: A): A
  }

  trait NumBool[A] extends Aux {
    def not(a: A): A
  }

  trait NumInt[A] extends Num[A] {
    def unary_~ (a: A): A

    def &       (a: A, b: A): A
    def |       (a: A, b: A): A
    def ^       (a: A, b: A): A

    def lcm     (a: A, b: A): A
    def gcd     (a: A, b: A): A
  }

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
  }

  trait NumDoubleC[C[_]] extends NumDouble[C[Double]] {
    def coin[Tx](a: C[Double])(implicit r: Random[Tx], tx: Tx): C[Boolean]
  }

  trait ToNum[C[_], A] extends Aux {
    def toInt   (a: A): C[Int]
    def toDouble(a: A): C[Double]

    def int   : NumInt    [C[Int]]
    def double: NumDoubleC[C]
  }

  type Id[B] = B

  trait ScalarToNum[A] extends ToNum[Id, A] {
    final def int   : NumInt    [Int] = IntTop
    final def double: NumDoubleC[Id]  = DoubleTop
  }

  trait ScalarEq[A] extends Eq[A] {
    final def eq  (a: A, b: A): Boolean = a == b
    final def neq (a: A, b: A): Boolean = a != b
  }

  trait SeqLike[A] {
    final protected def unOp[B](a: Seq[A])(op: A => B): Seq[B] = a.map(op)

    final protected def binOp[B](a: Seq[A], b: Seq[A])(op: (A, A) => B): Seq[B] = {
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

  trait SeqLikeEq[A] extends SeqLike[A] with EqC[Seq, Seq[A]] {
    protected val peer: Eq[A]

    def eq (a: Seq[A], b: Seq[A]): Seq[Boolean] = binOp(a, b)(peer.eq )
    def neq(a: Seq[A], b: Seq[A]): Seq[Boolean] = binOp(a, b)(peer.neq)
  }

  trait SeqLikeOrd[A] extends SeqLikeEq[A] with OrdC[Seq, Seq[A]] {
    protected val peer: Ord[A]

    def lt  (a: Seq[A], b: Seq[A]): Seq[Boolean] = binOp(a, b)(peer.lt  )
    def leq (a: Seq[A], b: Seq[A]): Seq[Boolean] = binOp(a, b)(peer.leq )
    def gt  (a: Seq[A], b: Seq[A]): Seq[Boolean] = binOp(a, b)(peer.gt  )
    def geq (a: Seq[A], b: Seq[A]): Seq[Boolean] = binOp(a, b)(peer.geq )
  }

  trait SeqLikeNum[A] extends SeqLikeOrd[A] with Num[Seq[A]] {
    override protected val peer: Num[A] with Ord[A]

    final def plus      (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.plus      )
    final def minus     (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.minus     )
    final def times     (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.times     )
    final def roundTo   (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.roundTo   )
    final def roundUpTo (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.roundUpTo )
    final def trunc     (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.trunc     )
    final def %         (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.%         )
    final def mod       (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.mod       )
    final def min       (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.min       )
    final def max       (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.max       )
    final def clip2     (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.clip2     )
    final def fold2     (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.fold2     )
    final def wrap2     (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.wrap2     )

    final def negate    (a: Seq[A]): Seq[A]   = unOp(a)(peer.negate )
    final def abs       (a: Seq[A]): Seq[A]   = unOp(a)(peer.abs    )
    final def signum    (a: Seq[A]): Seq[A]   = unOp(a)(peer.signum )

    final def squared   (a: Seq[A]): Seq[A]   = unOp(a)(peer.squared)
    final def cubed     (a: Seq[A]): Seq[A]   = unOp(a)(peer.cubed  )

    final def zero : Seq[A] = peer.zero :: Nil
    final def one  : Seq[A] = peer.one  :: Nil

    final def rand [Tx](a: Seq[A]           )(implicit r: Random[Tx], tx: Tx): Seq[A] = unOp (a   )(peer.rand [Tx])
    final def rand2[Tx](a: Seq[A]           )(implicit r: Random[Tx], tx: Tx): Seq[A] = unOp (a   )(peer.rand2[Tx])
    final def rrand[Tx](a: Seq[A], b: Seq[A])(implicit r: Random[Tx], tx: Tx): Seq[A] = binOp(a, b)(peer.rrand[Tx])

    final def fold(a: Seq[A], lo: Seq[A], hi: Seq[A]): Seq[A] = ternOp(a, lo, hi)(peer.fold)
  }

  trait SeqLikeNumFrac[A] extends SeqLikeNum[A] with NumFrac[Seq[A]] {
    override protected val peer: NumFrac[A] with Ord[A]

    final def floor     (a: Seq[A]): Seq[A] = unOp(a)(peer.floor      )
    final def ceil      (a: Seq[A]): Seq[A] = unOp(a)(peer.ceil       )
    final def frac      (a: Seq[A]): Seq[A] = unOp(a)(peer.frac       )
    final def reciprocal(a: Seq[A]): Seq[A] = unOp(a)(peer.reciprocal )

    final def div       (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.div)
  }

  trait SeqLikeNumDouble extends SeqLikeNumFrac[Double] with NumDoubleC[Seq] {
    final type A = Double

    override protected val peer: NumDoubleC[Id] with Ord[A]

    final def sqrt      (a: Seq[A]): Seq[A] = unOp(a)(peer.sqrt     )
    final def exp       (a: Seq[A]): Seq[A] = unOp(a)(peer.exp      )

    final def midicps   (a: Seq[A]): Seq[A] = unOp(a)(peer.midicps  )
    final def cpsmidi   (a: Seq[A]): Seq[A] = unOp(a)(peer.cpsmidi  )
    final def midiratio (a: Seq[A]): Seq[A] = unOp(a)(peer.midiratio)
    final def ratiomidi (a: Seq[A]): Seq[A] = unOp(a)(peer.ratiomidi)
    final def dbamp     (a: Seq[A]): Seq[A] = unOp(a)(peer.dbamp    )
    final def ampdb     (a: Seq[A]): Seq[A] = unOp(a)(peer.ampdb    )
    final def octcps    (a: Seq[A]): Seq[A] = unOp(a)(peer.octcps   )
    final def cpsoct    (a: Seq[A]): Seq[A] = unOp(a)(peer.cpsoct   )
    final def log       (a: Seq[A]): Seq[A] = unOp(a)(peer.log      )
    final def log2      (a: Seq[A]): Seq[A] = unOp(a)(peer.log2     )
    final def log10     (a: Seq[A]): Seq[A] = unOp(a)(peer.log10    )
    final def sin       (a: Seq[A]): Seq[A] = unOp(a)(peer.sin      )
    final def cos       (a: Seq[A]): Seq[A] = unOp(a)(peer.cos      )
    final def tan       (a: Seq[A]): Seq[A] = unOp(a)(peer.tan      )
    final def asin      (a: Seq[A]): Seq[A] = unOp(a)(peer.asin     )
    final def acos      (a: Seq[A]): Seq[A] = unOp(a)(peer.acos     )
    final def atan      (a: Seq[A]): Seq[A] = unOp(a)(peer.atan     )
    final def sinh      (a: Seq[A]): Seq[A] = unOp(a)(peer.sinh     )
    final def cosh      (a: Seq[A]): Seq[A] = unOp(a)(peer.cosh     )
    final def tanh      (a: Seq[A]): Seq[A] = unOp(a)(peer.tanh     )

    final def atan2     (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.atan2 )
    final def hypot     (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.hypot )
    final def hypotx    (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.hypotx)
    final def pow       (a: Seq[A], b: Seq[A]): Seq[A] = binOp(a, b)(peer.pow   )

    def coin[Tx](a: Seq[A])(implicit r: Random[Tx], tx: Tx): Seq[Boolean] = unOp(a)(peer.coin[Tx])
  }

  trait SeqLikeToNum[A] extends SeqLike[A] with ToNum[Seq, Seq[A]] {
    protected val peer: ScalarToNum[A]

    final type Shape[B] = Seq[B]

    final def toInt   (a: Seq[A]): Seq[Int]     = unOp(a)(peer.toInt   )
    final def toDouble(a: Seq[A]): Seq[Double]  = unOp(a)(peer.toDouble)

    final def int    : NumInt    [Seq[Int]] = IntSeqTop
    final def double : NumDoubleC[Seq]      = DoubleSeqTop
  }

  implicit object IntSeqTop
    extends NumInt      [Seq[Int]]
    with    OrdC        [Seq, Seq[Int]]
    with    SeqLikeNum  [Int]
    with    SeqLikeToNum[Int] {

    protected final val peer: IntTop.type = IntTop

    def unary_~(a: Seq[Int]): Seq[Int] = unOp(a)(peer.unary_~)

    def &   (a: Seq[Int], b: Seq[Int]): Seq[Int] = binOp(a, b)(peer.&)
    def |   (a: Seq[Int], b: Seq[Int]): Seq[Int] = binOp(a, b)(peer.|)
    def ^   (a: Seq[Int], b: Seq[Int]): Seq[Int] = binOp(a, b)(peer.^)

    def lcm (a: Seq[Int], b: Seq[Int]): Seq[Int] = binOp(a, b)(peer.lcm)
    def gcd (a: Seq[Int], b: Seq[Int]): Seq[Int] = binOp(a, b)(peer.gcd)

    final val id = 1
  }

  implicit object IntTop
    extends NumInt      [Int]
    with    ScalarEq    [Int]
    with    Ord         [Int]
    with    ScalarToNum [Int] {

    final val id = 0

    def zero   : Int = 0
    def one    : Int = 1

    def negate    (a: Int): Int     = -a
    def abs       (a: Int): Int     = ri.abs(a)
    def signum    (a: Int): Int     = ri.signum(a)

    def toInt     (a: Int): Int     = a
    def toDouble  (a: Int): Double  = a.toDouble

    def plus      (a: Int, b: Int): Int = a + b
    def minus     (a: Int, b: Int): Int = a - b
    def times     (a: Int, b: Int): Int = a * b
    def roundTo   (a: Int, b: Int): Int = if (b == 0) a else math.round(a.toDouble / b).toInt * b
    def roundUpTo (a: Int, b: Int): Int = ???
    def trunc     (a: Int, b: Int): Int = ???

    def %         (a: Int, b: Int): Int = a % b
    def mod       (a: Int, b: Int): Int = ri.mod(a, b)
    def min       (a: Int, b: Int): Int = ri.min(a, b)
    def max       (a: Int, b: Int): Int = ri.max(a, b)
    def lcm       (a: Int, b: Int): Int = ri.lcm(a, b)
    def gcd       (a: Int, b: Int): Int = ri.gcd(a, b)

    def clip2     (a: Int, b: Int): Int = ri.clip2(a, b)
    def fold2     (a: Int, b: Int): Int = ri.fold2(a, b)
    def wrap2     (a: Int, b: Int): Int = ri.wrap2(a, b)

    def unary_~   (a: Int): Int = ~a

    def squared   (a: Int): Int = a * a       // ri.squared: Long
    def cubed     (a: Int): Int = a * a * a   // ri2.cubed : Long

    def &         (a: Int, b: Int): Int = a & b
    def |         (a: Int, b: Int): Int = a | b
    def ^         (a: Int, b: Int): Int = a ^ b

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

  implicit object DoubleSeqTop
    extends SeqLikeNumFrac[Double] with SeqLikeToNum[Double] with SeqLikeNumDouble {

    protected final val peer: DoubleTop.type = DoubleTop

    final val id = 3
  }

  implicit object DoubleTop
    extends NumDoubleC  [Id]
    with    ScalarEq    [Double]
    with    Ord         [Double]
    with    ScalarToNum [Double] {

    final val id = 2

    def zero   : Double = 0.0
    def one    : Double = 1.0

    def negate    (a: Double): Double = -a
    def abs       (a: Double): Double = rd.abs(a)
    def signum    (a: Double): Double = rd.signum(a)

    def toInt     (a: Double): Int    = a.toInt
    def toDouble  (a: Double): Double = a

    def floor     (a: Double): Double = rd.floor    (a)
    def ceil      (a: Double): Double = rd.ceil     (a)
    def frac      (a: Double): Double = rd.frac     (a)
    def midicps   (a: Double): Double = rd.midicps  (a)
    def cpsmidi   (a: Double): Double = rd.cpsmidi  (a)
    def midiratio (a: Double): Double = rd.midiratio(a)
    def ratiomidi (a: Double): Double = rd.ratiomidi(a)
    def dbamp     (a: Double): Double = rd.dbamp    (a)
    def ampdb     (a: Double): Double = rd.ampdb    (a)
    def octcps    (a: Double): Double = rd.octcps   (a)
    def cpsoct    (a: Double): Double = rd.cpsoct   (a)
    def log       (a: Double): Double = rd.log      (a)
    def log2      (a: Double): Double = rd.log2     (a)
    def log10     (a: Double): Double = rd.log10    (a)
    def sin       (a: Double): Double = rd.sin      (a)
    def cos       (a: Double): Double = rd.cos      (a)
    def tan       (a: Double): Double = rd.tan      (a)
    def asin      (a: Double): Double = rd.asin     (a)
    def acos      (a: Double): Double = rd.acos     (a)
    def atan      (a: Double): Double = rd.atan     (a)
    def sinh      (a: Double): Double = rd.sinh     (a)
    def cosh      (a: Double): Double = rd.cosh     (a)
    def tanh      (a: Double): Double = rd.tanh     (a)

    def atan2     (a: Double, b: Double): Double = rd.atan2 (a, b)
    def hypot     (a: Double, b: Double): Double = rd.hypot (a, b)
    def hypotx    (a: Double, b: Double): Double = rd.hypotx(a, b)
    def pow       (a: Double, b: Double): Double = rd.pow   (a, b)

    def plus      (a: Double, b: Double): Double = rd.+(a, b)
    def minus     (a: Double, b: Double): Double = rd.-(a, b)
    def times     (a: Double, b: Double): Double = rd.*(a, b)
    def div       (a: Double, b: Double): Double = rd./(a, b)
    def roundTo   (a: Double, b: Double): Double = rd.roundTo(a, b)
    def roundUpTo (a: Double, b: Double): Double = rd.roundUpTo(a, b)
    def trunc     (a: Double, b: Double): Double = rd.trunc(a, b)

    def %         (a: Double, b: Double): Double = rd.%  (a, b)
    def mod       (a: Double, b: Double): Double = rd.mod(a, b)

    def min       (a: Double, b: Double): Double = rd.min(a, b)
    def max       (a: Double, b: Double): Double = rd.max(a, b)

    def clip2     (a: Double, b: Double): Double = rd.clip2(a, b)
    def fold2     (a: Double, b: Double): Double = rd.fold2(a, b)
    def wrap2     (a: Double, b: Double): Double = rd.wrap2(a, b)

    def sqrt(a: Double): Double = rd.sqrt(a)
    def exp (a: Double): Double = rd.exp (a)

    def squared (a: Double): Double = rd.squared(a)
    def cubed   (a: Double): Double = a * a * a

    def reciprocal(a: Double): Double = 1.0 / a

    def rand[Tx](a: Double)(implicit r: Random[Tx], tx: Tx): Double =
      r.nextDouble() * a

    def rand2[Tx](a: Double)(implicit r: Random[Tx], tx: Tx): Double =
      (r.nextDouble() * 2 - 1) * a

    def rrand[Tx](a: Double, b: Double)(implicit r: Random[Tx], tx: Tx): Double =
      r.nextDouble() * (b - a) + a

    def coin[Tx](a: Double)(implicit r: Random[Tx], tx: Tx): Boolean =
      r.nextDouble() < a

    def lt (a: Double, b: Double): Boolean = a <  b
    def leq(a: Double, b: Double): Boolean = a <= b
    def gt (a: Double, b: Double): Boolean = a >  b
    def geq(a: Double, b: Double): Boolean = a >= b

    def fold(a: Double, lo: Double, hi: Double): Double = rd.fold(a, lo, hi)
  }

  implicit object BooleanSeqTop
    extends NumBool[Seq[Boolean]]
    with SeqLikeEq    [Boolean]
    with SeqLikeToNum [Boolean] {

    final val peer: BooleanTop.type = BooleanTop

    final val id = 5

    type A = Boolean

    def not(a: Seq[A]): Seq[A] = unOp(a)(!_)
  }

  implicit object BooleanTop
    extends NumBool [Boolean]
    with ScalarEq   [Boolean]
    with ScalarToNum[Boolean] {

    final val id = 4

    def toInt   (a: Boolean): Int     = if (a) 1   else 0
    def toDouble(a: Boolean): Double  = if (a) 1.0 else 0.0

    def not(a: Boolean): Boolean = !a
  }

  implicit object intSeqWiden1 extends Widen2[Int, Seq[Int], Seq[Int]] {
    def widen1(a: Int     ): Seq[Int] = a :: Nil
    def widen2(a: Seq[Int]): Seq[Int] = a

    final val id = 0x100
  }

  implicit object intSeqWiden2 extends Widen2[Seq[Int], Int, Seq[Int]] {
    def widen1(a: Seq[Int]): Seq[Int] = a
    def widen2(a: Int     ): Seq[Int] = a :: Nil

    final val id = 0x101
  }

  implicit object doubleSeqWiden1 extends Widen2[Double, Seq[Double], Seq[Double]] {
    def widen1(a: Double     ): Seq[Double] = a :: Nil
    def widen2(a: Seq[Double]): Seq[Double] = a

    final val id = 0x102
  }

  implicit object doubleSeqWiden2 extends Widen2[Seq[Double], Double, Seq[Double]] {
    def widen1(a: Seq[Double]): Seq[Double] = a
    def widen2(a: Double     ): Seq[Double] = a :: Nil

    final val id = 0x103
  }

  implicit object intDoubleWiden1 extends Widen2[Int, Double, Double] {
    def widen1(a: Int    ): Double = a.toDouble
    def widen2(a: Double ): Double = a

    final val id = 0x104
  }

  implicit object intDoubleWiden2 extends Widen2[Double, Int, Double] {
    def widen1(a: Double ): Double = a
    def widen2(a: Int    ): Double = a.toDouble

    final val id = 0x105
  }
  ////////////////////////////

//  sealed trait StringTop extends CTop {
//    type COut = _String
//  }

  implicit object StringTop extends Eq[String] {
    def eq  (a: String, b: String): Boolean = a == b
    def neq (a: String, b: String): Boolean = a != b

    final val id = 10
  }

//  trait Tuple2Top[T1 <: Top, T2 <: Top] extends Top {
//    type Out[Tx] = (T1#Out[Tx], T2#Out[Tx])
//  }
}
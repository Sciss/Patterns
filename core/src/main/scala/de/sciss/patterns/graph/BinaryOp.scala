/*
 *  BinaryOp.scala
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
package graph

import de.sciss.patterns.Types.{Aux, EqC, Num, NumFrac, NumInt, OrdC, ToNum, Widen2}

import scala.language.higherKinds

object BinaryOp {
  sealed abstract class Op[A1, A2] extends ProductWithAux {
    type State[Tx]

    def prepare[Tx](ref: AnyRef)(implicit ctx: Context[Tx], tx: Tx): State[Tx]

    def next[Tx](a: A1, b: A1)(implicit state: State[Tx], tx: Tx): A2

    override final def productPrefix = s"BinaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  abstract class PureOp[A1, A2] extends Op[A1, A2] {
    final type State[_] = Unit

    final def prepare[Tx](ref: AnyRef)(implicit ctx: Context[Tx], tx: Tx): State[Tx] = ()

    def next[Tx](a: A1, b: A1)(implicit state: State[Tx], tx: Tx): A2 = apply(a, b)

    def apply(a: A1, b: A1): A2
  }

  // ---- (Num, Num) -> Num ----

  final case class Plus[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.plus(a, b)
    def name                  : String    = "Plus"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Minus[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.minus(a, b)
    def name                  : String    = "Minus"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Times[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.times(a, b)
    def name                  : String    = "Times"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Div[A]()(implicit num: NumFrac[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.div(a, b)
    def name                  : String    = "Div"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Mod[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.mod(a, b)
    def name                  : String    = "Mod"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class % [A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.%(a, b)
    def name                  : String    = "%"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  // ---- (Ord, Ord) -> Boolean ----

  /** Equal */
  final case class Eq[C[_], A]()(implicit eq: EqC[C, A]) extends PureOp[A, C[Boolean]] {
    def apply(a: A, b: A)     : C[Boolean]  = eq.eq(a, b)
    def name                  : String      = "Eq"
    private[patterns] def aux : List[Aux]   = Nil
  }

  /** Not equal */
  final case class Neq[C[_], A]()(implicit eq: EqC[C, A]) extends PureOp[A, C[Boolean]] {
    def apply(a: A, b: A)     : C[Boolean]  = eq.neq(a, b)
    def name                  : String      = "Neq"
    private[patterns] def aux : List[Aux]   = Nil
  }

  /** Less than */
  final case class Lt[C[_], A]()(implicit ord: OrdC[C, A]) extends PureOp[A, C[Boolean]] {
    def apply(a: A, b: A)     : C[Boolean]  = ord.lt(a, b)
    def name                  : String      = "Lt"
    private[patterns] def aux : List[Aux]   = ord :: Nil
  }

  /** Greater than */
  final case class Gt[C[_], A]()(implicit ord: OrdC[C, A]) extends PureOp[A, C[Boolean]] {
    def apply(a: A, b: A)     : C[Boolean]  = ord.gt(a, b)
    def name                  : String      = "Gt"
    private[patterns] def aux : List[Aux]   = ord :: Nil
  }

  /** Less than or equal */
  final case class Leq[C[_], A]()(implicit ord: OrdC[C, A]) extends PureOp[A, C[Boolean]] {
    def apply(a: A, b: A)     : C[Boolean]  = ord.leq(a, b)
    def name                  : String      = "Leq"
    private[patterns] def aux : List[Aux]   = ord :: Nil
  }

  /** Greater than or equal */
  final case class Geq[C[_], A]()(implicit ord: OrdC[C, A]) extends PureOp[A, C[Boolean]] {
    def apply(a: A, b: A)     : C[Boolean]  = ord.geq(a, b)
    def name                  : String      = "Geq"
    private[patterns] def aux : List[Aux]   = ord :: Nil
  }

  // ---- (Num, Num) -> Num ----

  final case class Min[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.min(a, b)
    def name                  : String    = "Min"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Max[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.max(a, b)
    def name                  : String    = "Max"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class BitAnd[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.&(a, b)
    def name                  : String    = "BitAnd"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class BitOr[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.|(a, b)
    def name                  : String    = "BitOr"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class BitXor[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.^(a, b)
    def name                  : String    = "BitXor"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Lcm[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.lcm(a, b)
    def name                  : String    = "Lcm"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Gcd[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.gcd(a, b)
    def name                  : String    = "Gcd"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class RoundTo[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.roundTo(a, b)
    def name                  : String    = "RoundTo"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class RoundUpTo[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.roundUpTo(a, b)
    def name                  : String    = "RoundUpTo"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Trunc[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.trunc(a, b)
    def name                  : String    = "Trunc"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Atan2[C[_], A]()(implicit num: ToNum[C, A]) extends PureOp[A, C[Double]] {
    def apply(a: A, b: A)     : C[Double] = num.double.atan2(num.toDouble(a), num.toDouble(b))
    def name                  : String    = "Atan2"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Hypot[C[_], A]()(implicit num: ToNum[C, A]) extends PureOp[A, C[Double]] {
    def apply(a: A, b: A)     : C[Double] = num.double.hypot(num.toDouble(a), num.toDouble(b))
    def name                  : String    = "Hypot"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Hypotx[C[_], A]()(implicit num: ToNum[C, A]) extends PureOp[A, C[Double]] {
    def apply(a: A, b: A)     : C[Double] = num.double.hypotx(num.toDouble(a), num.toDouble(b))
    def name                  : String    = "Hypotx"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Pow[C[_], A]()(implicit num: ToNum[C, A]) extends PureOp[A, C[Double]] {
    def apply(a: A, b: A)     : C[Double] = num.double.pow(num.toDouble(a), num.toDouble(b))
    def name                  : String    = "Pow"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

// XXX TODO:
//  LeftShift
//  RightShift

// XXX TODO:
//  Ring1
//  Ring2
//  Ring3
//  Ring4
//  Difsqr
//  Sumsqr
//  Sqrsum
//  Sqrdif
//  Absdif
//  Thresh
//  Amclip
//  Scaleneg

  final case class Clip2[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A): A = num.clip2(a, b)

    def name = "Clip2"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

//  XXX TODO: Excess

  final case class Fold2[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A): A = num.fold2(a, b)

    def name = "Fold2"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Wrap2[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A): A = num.wrap2(a, b)

    def name = "Wrap2"

    private[patterns] def aux: List[Aux] = num :: Nil
  }
}

final case class BinaryOp[A1, A2, A3, A](op: BinaryOp.Op[A3, A], a: Pat[A1], b: Pat[A2])
                                        (implicit w: Widen2[A1, A2, A3])
  extends Pattern[A] { pat =>

  override private[patterns] def aux: List[Aux] = w :: Nil

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl[Tx](tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val aT = t(a)
    val bT = t(b)
    if (aT.eq(a) && bT.eq(b)) this else copy(a = aT, b = bT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val aStream = a.expand(ctx, tx0)
    private[this] val bStream = b.expand(ctx, tx0)

    private[this] implicit val state: op.State[Tx]  = op.prepare(ref)(ctx, tx0)

    def reset()(implicit tx: Tx): Unit = {
      aStream.reset()
      bStream.reset()
    }

    def hasNext(implicit tx: Tx): Boolean =
      aStream.hasNext && bStream.hasNext

    def next()(implicit tx: Tx): A = {
      val aVal = w.widen1(aStream.next())
      val bVal = w.widen2(bStream.next())
      op.next(aVal, bVal)
    }
  }
}

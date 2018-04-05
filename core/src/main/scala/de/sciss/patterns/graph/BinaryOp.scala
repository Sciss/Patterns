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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.Types.{Aux, Num, NumDouble, NumFrac, NumInt, Ord, Widen2}

import scala.language.higherKinds

object BinaryOp {
  sealed abstract class Op[A1, A2] extends ProductWithAux {
    type State[Tx]

    def prepare[S <: Base[S]](ref: AnyRef)(implicit ctx: Context[S], tx: S#Tx): State[S#Tx]

    def next[S <: Base[S]](a: A1, b: A1)(implicit state: State[S#Tx], tx: S#Tx): A2

    override final def productPrefix = s"BinaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  abstract class PureOp[A1, A2] extends Op[A1, A2] {
    final type State[_] = Unit

    final def prepare[S <: Base[S]](ref: AnyRef)(implicit ctx: Context[S], tx: S#Tx): State[S#Tx] = ()

    def next[S <: Base[S]](a: A1, b: A1)(implicit state: State[S#Tx], tx: S#Tx): A2 = apply(a, b)

    def apply(a: A1, b: A1): A2
  }

  // ---- (Num, Num) -> Num ----

  final case class Plus[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.+(a, b)
    def name                  : String    = "Plus"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Minus[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.-(a, b)
    def name                  : String    = "Minus"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Times[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.*(a, b)
    def name                  : String    = "Times"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  /** Division, _not_ integer division */
  final case class Div[A]()(implicit num: NumFrac[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num./(a, b)
    def name                  : String    = "Div"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class % [A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.%(a, b)
    def name                  : String    = "%"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Mod[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.mod(a, b)
    def name                  : String    = "Mod"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  // ---- (Ord, Ord) -> Boolean ----

  /** Equal */
  final case class Eq[A, B]()(implicit eq: Types.Eq[A] { type Boolean = B }) extends PureOp[A, B] {
    def apply(a: A, b: A)     : B           = eq.eq(a, b)
    def name                  : String      = "Eq"
    private[patterns] def aux : List[Aux]   = eq :: Nil
  }

  /** Not equal */
  final case class Neq[A, B]()(implicit eq: Types.Eq[A] { type Boolean = B}) extends PureOp[A, B] {
    def apply(a: A, b: A)     : B           = eq.neq(a, b)
    def name                  : String      = "Neq"
    private[patterns] def aux : List[Aux]   = eq :: Nil
  }

  /** Less than */
  final case class Lt[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends PureOp[A, B] {
    def apply(a: A, b: A)     : B           = ord.lt(a, b)
    def name                  : String      = "Lt"
    private[patterns] def aux : List[Aux]   = ord :: Nil
  }

  /** Greater than */
  final case class Gt[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends PureOp[A, B] {
    def apply(a: A, b: A)     : B           = ord.gt(a, b)
    def name                  : String      = "Gt"
    private[patterns] def aux : List[Aux]   = ord :: Nil
  }

  /** Less than or equal */
  final case class Leq[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends PureOp[A, B] {
    def apply(a: A, b: A)     : B           = ord.leq(a, b)
    def name                  : String      = "Leq"
    private[patterns] def aux : List[Aux]   = ord :: Nil
  }

  /** Greater than or equal */
  final case class Geq[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends PureOp[A, B] {
    def apply(a: A, b: A)     : B           = ord.geq(a, b)
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

  final case class Atan2[A]()(implicit num: NumDouble[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.atan2(a, b)
    def name                  : String    = "Atan2"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Hypot[A]()(implicit num: NumDouble[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.hypot(a, b)
    def name                  : String    = "Hypot"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Hypotx[A]()(implicit num: NumDouble[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.hypotx(a, b)
    def name                  : String    = "Hypotx"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Pow[A]()(implicit num: NumDouble[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.pow(a, b)
    def name                  : String    = "Pow"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class LeftShift[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.<<(a, b)
    def name                  : String    = "LeftShift"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class RightShift[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.>>(a, b)
    def name                  : String    = "RightShift"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class UnsignedRightShift[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.>>>(a, b)
    def name                  : String    = "UnsignedRightShift"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  //  Ring1
  //  Ring2
  //  Ring3
  //  Ring4

  final case class Difsqr[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.difsqr(a, b)
    def name                  : String    = "Difsqr"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Sumsqr[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.sumsqr(a, b)
    def name                  : String    = "Sumsqr"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Sqrsum[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.sqrsum(a, b)
    def name                  : String    = "Sqrsum"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Sqrdif[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.sqrdif(a, b)
    def name                  : String    = "Sqrdif"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Absdif[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.absdif(a, b)
    def name                  : String    = "Absdif"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

//  Thresh
//  Amclip
//  Scaleneg

  final case class Clip2[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.clip2(a, b)
    def name                  : String    = "Clip2"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Excess[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.excess(a, b)
    def name                  : String    = "Excess"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Fold2[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.fold2(a, b)
    def name                  : String    = "Fold2"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Wrap2[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.wrap2(a, b)
    def name                  : String    = "Wrap2"
    private[patterns] def aux : List[Aux] = num :: Nil
  }
}

final case class BinaryOp[A1, A2, A3, A](op: BinaryOp.Op[A3, A], a: Pat[A1], b: Pat[A2])
                                        (implicit w: Widen2[A1, A2, A3])
  extends Pattern[A] { pat =>

  override private[patterns] def aux: List[Aux] = w :: Nil

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val aT = t(a)
    val bT = t(b)
    if (aT.eq(a) && bT.eq(b)) this else copy(a = aT, b = bT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, A] {
    private[this] val aStream = a.expand(ctx, tx0)
    private[this] val bStream = b.expand(ctx, tx0)

    private[this] implicit val state: op.State[S#Tx]  = op.prepare(ref)(ctx, tx0)

    def reset()(implicit tx: S#Tx): Unit = {
      aStream.reset()
      bStream.reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
      aStream.hasNext && bStream.hasNext

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      val aVal = w.widen1(aStream.next())
      val bVal = w.widen2(bStream.next())
      op.next(aVal, bVal)
    }
  }
}

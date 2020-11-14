/*
 *  BinaryOp.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package graph

import de.sciss.lucre.Adjunct.{Num, NumDouble, NumFrac, NumInt, Ord, Widen2}
import de.sciss.lucre.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.Exec
import de.sciss.patterns.stream.BinaryOpImpl
import de.sciss.serial.{DataInput, DataOutput}

object BinaryOp {
  sealed abstract class Op[A1, A2] extends ProductWithAdjuncts {
    type State[T <: Exec[T]]

    def readState   [T <: Exec[T]](in: DataInput)(implicit tx: T): State[T]
    def writeState  [T <: Exec[T]](s: State[T], out: DataOutput): Unit
    def disposeState[T <: Exec[T]](s: State[T])(implicit tx: T): Unit

    def copyState[T <: Exec[T], Out <: Exec[Out]](s: State[T])(implicit tx: T, txOut: Out): State[Out]

    def prepare[T <: Exec[T]](ref: AnyRef)(implicit ctx: Context[T], tx: T): State[T]

    def next[T <: Exec[T]](a: A1, b: A1)(implicit state: State[T], tx: T): A2

    override final def productPrefix = s"BinaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  abstract class PureOp[A1, A2] extends Op[A1, A2] {
    final type State[T <: Exec[T]] = Unit

    final def readState   [T <: Exec[T]](in: DataInput)(implicit tx: T): State[T] = ()
    final def writeState  [T <: Exec[T]](s: State[T], out: DataOutput): Unit = ()
    final def disposeState[T <: Exec[T]](s: State[T])(implicit tx: T): Unit = ()

    def copyState[T <: Exec[T], Out <: Exec[Out]](s: State[T])(implicit tx: T, txOut: Out): State[Out] = ()

    final def prepare[T <: Exec[T]](ref: AnyRef)(implicit ctx: Context[T], tx: T): State[T] = ()

    def next[T <: Exec[T]](a: A1, b: A1)(implicit state: State[T], tx: T): A2 = apply(a, b)

    def apply(a: A1, b: A1): A2
  }

  // ---- (Num, Num) -> Num ----

  final case class Plus[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.plus(a, b)
    def name                  : String    = "Plus"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Minus[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.minus(a, b)
    def name                  : String    = "Minus"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Times[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.times(a, b)
    def name                  : String    = "Times"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  /** Division, _not_ integer division */
  final case class Div[A]()(implicit num: NumFrac[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.div(a, b)
    def name                  : String    = "Div"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class ModJ[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.rem(a, b)
    def name                  : String    = "ModJ"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Mod[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.mod(a, b)
    def name                  : String    = "Mod"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  // ---- (Ord, Ord) -> Boolean ----

  /** Equal */
  final case class Eq[A, B]()(implicit eq: Adjunct.Eq[A] { type Boolean = B }) extends PureOp[A, B] {
    def apply(a: A, b: A)     : B           = eq.eq(a, b)
    def name                  : String      = "Eq"
    override def adjuncts: List[Adjunct]   = eq :: Nil
  }

  /** Not equal */
  final case class Neq[A, B]()(implicit eq: Adjunct.Eq[A] { type Boolean = B}) extends PureOp[A, B] {
    def apply(a: A, b: A)     : B           = eq.neq(a, b)
    def name                  : String      = "Neq"
    override def adjuncts: List[Adjunct]   = eq :: Nil
  }

  /** Less than */
  final case class Lt[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends PureOp[A, B] {
    def apply(a: A, b: A)     : B           = ord.lt(a, b)
    def name                  : String      = "Lt"
    override def adjuncts: List[Adjunct]   = ord :: Nil
  }

  /** Greater than */
  final case class Gt[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends PureOp[A, B] {
    def apply(a: A, b: A)     : B           = ord.gt(a, b)
    def name                  : String      = "Gt"
    override def adjuncts: List[Adjunct]   = ord :: Nil
  }

  /** Less than or equal */
  final case class Leq[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends PureOp[A, B] {
    def apply(a: A, b: A)     : B           = ord.lteq(a, b)
    def name                  : String      = "Leq"
    override def adjuncts: List[Adjunct]   = ord :: Nil
  }

  /** Greater than or equal */
  final case class Geq[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends PureOp[A, B] {
    def apply(a: A, b: A)     : B           = ord.gteq(a, b)
    def name                  : String      = "Geq"
    override def adjuncts: List[Adjunct]   = ord :: Nil
  }

  // ---- (Num, Num) -> Num ----

  final case class Min[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.min(a, b)
    def name                  : String    = "Min"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Max[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.max(a, b)
    def name                  : String    = "Max"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class BitAnd[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.and(a, b)
    def name                  : String    = "BitAnd"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class BitOr[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.or(a, b)
    def name                  : String    = "BitOr"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class BitXor[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.xor(a, b)
    def name                  : String    = "BitXor"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Lcm[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.lcm(a, b)
    def name                  : String    = "Lcm"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Gcd[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.gcd(a, b)
    def name                  : String    = "Gcd"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class RoundTo[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.roundTo(a, b)
    def name                  : String    = "RoundTo"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class RoundUpTo[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.roundUpTo(a, b)
    def name                  : String    = "RoundUpTo"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Trunc[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.trunc(a, b)
    def name                  : String    = "Trunc"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Atan2[A]()(implicit num: NumDouble[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.atan2(a, b)
    def name                  : String    = "Atan2"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Hypot[A]()(implicit num: NumDouble[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.hypot(a, b)
    def name                  : String    = "Hypot"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Hypotx[A]()(implicit num: NumDouble[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.hypotApx(a, b)
    def name                  : String    = "Hypotx"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Pow[A]()(implicit num: NumDouble[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.pow(a, b)
    def name                  : String    = "Pow"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class LeftShift[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.shiftLeft(a, b)
    def name                  : String    = "LeftShift"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class RightShift[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.shiftRight(a, b)
    def name                  : String    = "RightShift"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class UnsignedRightShift[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.unsignedShiftRight(a, b)
    def name                  : String    = "UnsignedRightShift"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  //  Ring1
  //  Ring2
  //  Ring3
  //  Ring4

  final case class Difsqr[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.difSqr(a, b)
    def name                  : String    = "Difsqr"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Sumsqr[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.sumSqr(a, b)
    def name                  : String    = "Sumsqr"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Sqrsum[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.sqrSum(a, b)
    def name                  : String    = "Sqrsum"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Sqrdif[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.sqrDif(a, b)
    def name                  : String    = "Sqrdif"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Absdif[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.absDif(a, b)
    def name                  : String    = "Absdif"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

//  Thresh
//  Amclip
//  Scaleneg

  final case class Clip2[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.clip2(a, b)
    def name                  : String    = "Clip2"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Excess[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.excess(a, b)
    def name                  : String    = "Excess"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Fold2[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.fold2(a, b)
    def name                  : String    = "Fold2"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Wrap2[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A)     : A         = num.wrap2(a, b)
    def name                  : String    = "Wrap2"
    override def adjuncts: List[Adjunct] = num :: Nil
  }
}

final case class BinaryOp[A1, A2, A3, A](op: BinaryOp.Op[A3, A], a: Pat[A1], b: Pat[A2])
                                        (implicit val widen: Widen2[A1, A2, A3])
  extends Pattern[A] with ProductWithAdjuncts { pat =>

  override def adjuncts: List[Adjunct] = widen :: Nil

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    BinaryOpImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val aT = t(a)
    val bT = t(b)
    if (aT.eq(a) && bT.eq(b)) this else copy(a = aT, b = bT)
  }
}

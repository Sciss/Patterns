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

import de.sciss.patterns.Types.{Aux, Num, NumFrac, Ord, Widen}

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
    def apply(a: A, b: A): A = num.plus(a, b)

    def name = "Plus"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Minus[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A): A = num.minus(a, b)

    def name = "Minus"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Times[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A): A = num.times(a, b)

    def name = "Times"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Div[A]()(implicit num: NumFrac[A]) extends PureOp[A, A] {
    def apply(a: A, b: A): A = num.div(a, b)

    def name = "Div"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class RoundTo[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A): A = num.times(a, b)

    def name = "RoundTo"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class % [A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A): A = num.%(a, b)

    def name = "%"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Mod[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A, b: A): A = num.mod(a, b)

    def name = "Mod"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  // ---- (Ord, Ord) -> Boolean ----

  /** Less than or equal */
  final case class Leq[A]()(implicit ord: Ord[A]) extends PureOp[A, Boolean] {
    def apply(a: A, b: A): Boolean = ord.leq(a, b)

    def name = "Leq"

    private[patterns] def aux: List[Aux] = ord :: Nil
  }

  /** Less than */
  final case class Lt[A]()(implicit ord: Ord[A]) extends PureOp[A, Boolean] {
    def apply(a: A, b: A): Boolean = ord.lt(a, b)

    def name = "Lt"

    private[patterns] def aux: List[Aux] = ord :: Nil
  }

  /** Greater than or equal */
  final case class Geq[A]()(implicit ord: Ord[A]) extends PureOp[A, Boolean] {
    def apply(a: A, b: A): Boolean = ord.geq(a, b)

    def name = "Geq"

    private[patterns] def aux: List[Aux] = ord :: Nil
  }

  /** Greater than */
  final case class Gt[A]()(implicit ord: Ord[A]) extends PureOp[A, Boolean] {
    def apply(a: A, b: A): Boolean = ord.gt(a, b)

    def name = "Gt"

    private[patterns] def aux: List[Aux] = ord :: Nil
  }

  /** Equal */
  final case class Eq[A]() extends PureOp[A, Boolean] {
    def apply(a: A, b: A): Boolean = a == b

    def name = "Eq"

    private[patterns] def aux: List[Aux] = Nil
  }

  /** Not equal */
  final case class Neq[A]() extends PureOp[A, Boolean] {
    def apply(a: A, b: A): Boolean = a != b

    def name = "Neq"

    private[patterns] def aux: List[Aux] = Nil
  }
}
final case class BinaryOp[A1, A2, A3, A](op: BinaryOp.Op[A3, A], a: Pat[A1], b: Pat[A2])
                                        (implicit widen: Widen[A1, A2, A3])
  extends Pattern[A] { pat =>

  override private[patterns] def aux: List[Aux] = widen :: Nil

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
      val aVal = widen.lift1(aStream.next())
      val bVal = widen.lift2(bStream.next())
      op.next(aVal, bVal)
    }
  }
}

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

import de.sciss.patterns.Types.{Aux, BooleanTop, Bridge, Num, NumFrac, NumIntegral, Ord, Top}

object BinaryOp {
  sealed abstract class Op[T1 <: Top, T2 <: Top] extends ProductWithAux {
    def apply[Tx](a: T1#Out[Tx], b: T1#Out[Tx]): T2#Out[Tx]

    override final def productPrefix = s"BinaryOp$$$name"

    def name: String
  }

  // ---- (Num, Num) -> Num ----

  final case class Plus[T <: Top]()(implicit num: Num[T]) extends Op[T, T] {
    def apply[Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx] = num.plus(a, b)

    def name = "Plus"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Minus[T <: Top]()(implicit num: Num[T]) extends Op[T, T] {
    def apply[Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx] = num.minus(a, b)

    def name = "Minus"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Times[T <: Top]()(implicit num: Num[T]) extends Op[T, T] {
    def apply[Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx] = num.times(a, b)

    def name = "Times"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class RoundTo[T <: Top]()(implicit num: Num[T]) extends Op[T, T] {
    def apply[Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx] = num.times(a, b)

    def name = "RoundTo"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Modulus[T <: Top]()(implicit num: NumIntegral[T]) extends Op[T, T] {
    def apply[Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx] = num.rem(a, b)

    def name = "Modulus"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  // ---- (Ord, Ord) -> Boolean ----

  /** Less than or equal */
  final case class Leq[T <: Top]()(implicit ord: Ord[T]) extends Op[T, BooleanTop] {
    def apply[Tx](a: T#Out[Tx], b: T#Out[Tx]): Boolean = ord.leq(a, b)

    def name = "Leq"

    private[patterns] def aux: List[Aux] = ord :: Nil
  }

  /** Less than */
  final case class Lt[T <: Top]()(implicit ord: Ord[T]) extends Op[T, BooleanTop] {
    def apply[Tx](a: T#Out[Tx], b: T#Out[Tx]): Boolean = ord.lt(a, b)

    def name = "Lt"

    private[patterns] def aux: List[Aux] = ord :: Nil
  }

  /** Greater than or equal */
  final case class Geq[T <: Top]()(implicit ord: Ord[T]) extends Op[T, BooleanTop] {
    def apply[Tx](a: T#Out[Tx], b: T#Out[Tx]): Boolean = ord.geq(a, b)

    def name = "Geq"

    private[patterns] def aux: List[Aux] = ord :: Nil
  }

  /** Greater than */
  final case class Gt[T <: Top]()(implicit ord: Ord[T]) extends Op[T, BooleanTop] {
    def apply[Tx](a: T#Out[Tx], b: T#Out[Tx]): Boolean = ord.gt(a, b)

    def name = "Gt"

    private[patterns] def aux: List[Aux] = ord :: Nil
  }
}
final case class BinaryOp[T1 <: Top, T2 <: Top, T3 <: Top, T <: Top](op: BinaryOp.Op[T3, T], a: Pat[T1], b: Pat[T2])
                                                                    (implicit br: Bridge[T1, T2, T3])
  extends Pattern[T] {

  override private[patterns] def aux: List[Aux] = br :: Nil

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = {
    val ai = a.expand.map(br.lift1)
    val bi = b.expand.map(br.lift2)
    (ai zip bi).map { case (av, bv) => op(av, bv) }
  }
}

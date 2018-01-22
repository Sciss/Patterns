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

import de.sciss.patterns.Types.{Aux, Bridge, Num, Top}

object BinaryOp {
  sealed abstract class Op[T <: Top] extends ProductWithAux {
    def apply[Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx]

    override final def productPrefix = s"BinaryOp$$$name"

    def name: String
  }

  final case class Plus[T <: Top]()(implicit num: Num[T]) extends Op[T] {
    def apply[Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx] = num.plus(a, b)

    def name = "Plus"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Times[T <: Top]()(implicit num: Num[T]) extends Op[T] {
    def apply[Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx] = num.times(a, b)

    def name = "Times"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class RoundTo[T <: Top]()(implicit num: Num[T]) extends Op[T] {
    def apply[Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx] = num.times(a, b)

    def name = "RoundTo"

    private[patterns] def aux: List[Aux] = num :: Nil
  }
}
final case class BinaryOp[T1 <: Top, T2 <: Top, T <: Top](op: BinaryOp.Op[T], a: Pat[T1], b: Pat[T2])
                                                         (implicit br: Bridge[T1, T2, T])
  extends Pattern[T] {

  override private[patterns] def aux: List[Aux] = br :: Nil

  def iterator[Tx](implicit ctx: Context[Tx]): Stream[Tx, T#Out[Tx]] = {
    val ai = a.expand.map(br.lift1)
    val bi = b.expand.map(br.lift2)
    (ai zip bi).map { case (av, bv) => op(av, bv) }
  }
}

/*
 *  UnaryOp.scala
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

import de.sciss.patterns.Types.{Aux, ToNum}

object UnaryOp {
  sealed abstract class Op[A1, A2] extends ProductWithAux {
    def apply(a: A1): A2

    override final def productPrefix = s"UnaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  final case class ToInt[A]()(implicit num: ToNum[A]) extends Op[A, Int] {
    def apply(a: A): Int = num.toInt(a)

    def name = "ToInt"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class ToDouble[A]()(implicit num: ToNum[A]) extends Op[A, Double] {
    def apply(a: A): Double = num.toDouble(a)

    def name = "ToDouble"

    private[patterns] def aux: List[Aux] = num :: Nil
  }
}
final case class UnaryOp[A1, A](op: UnaryOp.Op[A1, A], a: Pat[A1])
  extends Pattern[A] {

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = {
    val ai = a.expand
    ai.map { av => op(av) }
  }

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val aT = t(a)
    if (aT.eq(a)) this else copy(a = aT)
  }
}

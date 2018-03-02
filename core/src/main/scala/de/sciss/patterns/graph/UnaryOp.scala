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

import de.sciss.patterns.Types.{Aux, Num, NumBool, NumFrac, ToNum}

object UnaryOp {
  sealed abstract class Op[A1, A2] extends ProductWithAux {
    def apply(a: A1): A2

    override final def productPrefix = s"UnaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  // ---- analogous to UGens ----

  final case class Neg[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A): A = num.negate(a)

    def name = "Neg"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Not[A]()(implicit num: NumBool[A]) extends Op[A, A] {
    def apply(a: A): A = num.not(a)

    def name = "Not"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Abs[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A): A = num.abs(a)

    def name = "Abs"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class ToDouble[A]()(implicit num: ToNum[A]) extends Op[A, Double] {
    def apply(a: A): Double = num.toDouble(a)

    def name = "ToDouble"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class ToInt[A]()(implicit num: ToNum[A]) extends Op[A, Int] {
    def apply(a: A): Int = num.toInt(a)

    def name = "ToInt"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Ceil[A]()(implicit num: NumFrac[A]) extends Op[A, A] {
    def apply(a: A): A = num.ceil(a)

    def name = "Ceil"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Floor[A]()(implicit num: NumFrac[A]) extends Op[A, A] {
    def apply(a: A): A = num.floor(a)

    def name = "Floor"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Frac[A]()(implicit num: NumFrac[A]) extends Op[A, A] {
    def apply(a: A): A = num.frac(a)

    def name = "Frac"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

//  final case class Signum[A]()(implicit num: Num[A]) extends Op[A, A] {
//    def apply(a: A): A = num.signum(a)
//
//    def name = "Signum"
//
//    private[patterns] def aux: List[Aux] = num :: Nil
//  }

  final case class Squared[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A): A = num.times(a, a)

    def name = "Squared"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Cubed[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A): A = num.times(num.times(a, a), a)

    def name = "Cubed"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

//  final case class Sqrt[A]()(implicit num: Num[A]) extends Op[A, A] {
//    def apply(a: A): A = num.sqrt(a)
//
//    def name = "Sqrt"
//
//    private[patterns] def aux: List[Aux] = num :: Nil
//  }

//  final case class Exp[A]()(implicit num: Num[A]) extends Op[A, A] {
//    def apply(a: A): A = num.exp(a)
//
//    def name = "Exp"
//
//    private[patterns] def aux: List[Aux] = num :: Nil
//  }

  final case class Reciprocal[A]()(implicit num: NumFrac[A]) extends Op[A, A] {
    def apply(a: A): A = num.div(num.one, a)

    def name = "Reciprocal"

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

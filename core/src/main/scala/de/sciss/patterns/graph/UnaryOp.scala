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

import scala.language.higherKinds

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

  final case class ToDouble[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.toDouble(a)

    def name = "ToDouble"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class ToInt[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Int]] {
    def apply(a: A): C[Int] = num.toInt(a)

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

  final case class Sqrt[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.sqrt(num.toDouble(a))

    def name = "Sqrt"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Exp[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.exp(num.toDouble(a))

    def name = "Exp"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Reciprocal[A]()(implicit num: NumFrac[A]) extends Op[A, A] {
    def apply(a: A): A = num.div(num.one, a)

    def name = "Reciprocal"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Midicps[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.midicps(num.toDouble(a))

    def name = "Midicps"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Cpsmidi[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.cpsmidi(num.toDouble(a))

    def name = "Cpsmidi"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Midiratio[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.midiratio(num.toDouble(a))

    def name = "Midiratio"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Ratiomidi[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.ratiomidi(num.toDouble(a))

    def name = "Ratiomidi"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Dbamp[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.dbamp(num.toDouble(a))

    def name = "Dbamp"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Ampdb[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.ampdb(num.toDouble(a))

    def name = "Ampdb"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Octcps[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.octcps(num.toDouble(a))

    def name = "Octcps"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Cpsoct[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.cpsoct(num.toDouble(a))

    def name = "Cpsoct"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Log[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.log(num.toDouble(a))

    def name = "Log"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Log2[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.log2(num.toDouble(a))

    def name = "Log2"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Log10[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.log10(num.toDouble(a))

    def name = "Log10"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Sin[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.sin(num.toDouble(a))

    def name = "Sin"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Cos[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.cos(num.toDouble(a))

    def name = "Cos"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Tan[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.tan(num.toDouble(a))

    def name = "Tan"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Asin[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.asin(num.toDouble(a))

    def name = "Asin"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Acos[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.acos(num.toDouble(a))

    def name = "Acos"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Atan[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.atan(num.toDouble(a))

    def name = "Atan"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Sinh[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.sinh(num.toDouble(a))

    def name = "Sinh"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Cosh[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.cosh(num.toDouble(a))

    def name = "Cosh"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Tanh[C[_], A]()(implicit num: ToNum[C, A]) extends Op[A, C[Double]] {
    def apply(a: A): C[Double] = num.double.tanh(num.toDouble(a))

    def name = "Tanh"

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

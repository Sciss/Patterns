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

import de.sciss.patterns.Types.{Aux, Num, NumBool, NumDoubleC, NumFrac, ToNum, Widen}

import scala.language.higherKinds

object UnaryOp {
  sealed abstract class Op[A1, A2] extends ProductWithAux {
    type State[Tx]

    def prepare[Tx](ref: AnyRef)(implicit ctx: Context[Tx], tx: Tx): State[Tx]

    def next[Tx](a: A1)(implicit state: State[Tx], tx: Tx): A2

    override final def productPrefix = s"UnaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  abstract class PureOp[A1, A2] extends Op[A1, A2] {
    final type State[_] = Unit

    final def prepare[Tx](ref: AnyRef)(implicit ctx: Context[Tx], tx: Tx): State[Tx] = ()

    def next[Tx](a: A1)(implicit state: State[Tx], tx: Tx): A2 = apply(a)

    def apply(a: A1): A2
  }

  abstract class RandomOp[A1, A2] extends Op[A1, A2] {
    final type State[Tx] = Random[Tx]

    final def prepare[Tx](ref: AnyRef)(implicit ctx: Context[Tx], tx: Tx): State[Tx] = ctx.mkRandom(ref)
  }

  // ---- analogous to UGens ----

  final case class Neg[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.negate(a)
    def name                  : String    = "Neg"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Not[A]()(implicit num: NumBool[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.not(a)
    def name                  : String    = "Not"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Abs[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.abs(a)
    def name                  : String    = "Abs"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class ToDouble[C[_], A]()(implicit to: ToNum[C, A]) extends PureOp[A, C[Double]] {
    def apply(a: A)           : C[Double] = to.toDouble(a)
    def name                  : String    = "ToDouble"
    private[patterns] def aux : List[Aux] = to :: Nil
  }

  final case class ToInt[C[_], A]()(implicit to: ToNum[C, A]) extends PureOp[A, C[Int]] {
    def apply(a: A): C[Int] = to.toInt(a)
    def name = "ToInt"
    private[patterns] def aux: List[Aux] = to :: Nil
  }

  final case class Ceil[A]()(implicit num: NumFrac[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.ceil(a)
    def name                  : String    = "Ceil"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Floor[A]()(implicit num: NumFrac[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.floor(a)
    def name                  : String    = "Floor"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Frac[A]()(implicit num: NumFrac[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.frac(a)
    def name                  : String    = "Frac"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Signum[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.signum(a)
    def name                  : String    = "Signum"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Squared[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.squared(a)
    def name                  : String    = "Squared"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Cubed[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.cubed(a)
    def name                  : String    = "Cubed"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Sqrt[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A)           : C[Double] = d.sqrt(w.widen1(a))
    def name                  : String    = "Sqrt"
    private[patterns] def aux : List[Aux] = w :: d :: Nil
  }

  final case class Exp[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A)           : C[Double] = d.exp(w.widen1(a))
    def name                  : String    = "Exp"
    private[patterns] def aux : List[Aux] = w :: d :: Nil
  }

  final case class Reciprocal[C[_], A, A1]()(implicit w: Widen[A1, A], num: NumFrac[A]) extends PureOp[A1, A] {
    def apply(a: A1)          : A         = num.reciprocal(w.widen1(a))
    def name                  : String    = "Reciprocal"
    private[patterns] def aux : List[Aux] = w :: num :: Nil
  }

  final case class Midicps[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A)           : C[Double] = d.midicps(w.widen1(a))
    def name                  : String    = "Midicps"
    private[patterns] def aux : List[Aux] = w :: d :: Nil
  }

  final case class Cpsmidi[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A)           : C[Double] = d.cpsmidi(w.widen1(a))
    def name                  : String    = "Cpsmidi"
    private[patterns] def aux : List[Aux] = w :: d :: Nil
  }

  final case class Midiratio[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A)           : C[Double] = d.midiratio(w.widen1(a))
    def name                  : String    = "Midiratio"
    private[patterns] def aux : List[Aux] = w :: d :: Nil
  }

  final case class Ratiomidi[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A)           : C[Double] = d.ratiomidi(w.widen1(a))
    def name                  : String    = "Ratiomidi"
    private[patterns] def aux : List[Aux] = w :: d :: Nil
  }

  final case class Dbamp[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A)           : C[Double] = d.dbamp(w.widen1(a))
    def name                  : String    = "Dbamp"
    private[patterns] def aux : List[Aux] = w :: d :: Nil
  }

  final case class Ampdb[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A)           : C[Double] = d.ampdb(w.widen1(a))
    def name                  : String    = "Ampdb"
    private[patterns] def aux : List[Aux] = w :: d :: Nil
  }

  final case class Octcps[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A)           : C[Double] = d.octcps(w.widen1(a))
    def name                  : String    = "Octcps"
    private[patterns] def aux : List[Aux] = w :: d :: Nil
  }

  final case class Cpsoct[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A)           : C[Double] = d.cpsoct(w.widen1(a))
    def name                  : String    = "Cpsoct"
    private[patterns] def aux : List[Aux] = w :: d :: Nil
  }

  final case class Log[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A)           : C[Double] = d.log(w.widen1(a))
    def name                  : String    = "Log"
    private[patterns] def aux : List[Aux] = w :: d :: Nil
  }

  final case class Log2[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A): C[Double] = d.log2(w.widen1(a))
    def name = "Log2"
    private[patterns] def aux: List[Aux] = w :: d :: Nil
  }

  final case class Log10[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A): C[Double] = d.log10(w.widen1(a))
    def name = "Log10"
    private[patterns] def aux: List[Aux] = w :: d :: Nil
  }

  final case class Sin[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A): C[Double] = d.sin(w.widen1(a))
    def name = "Sin"
    private[patterns] def aux: List[Aux] = w :: d :: Nil
  }

  final case class Cos[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A): C[Double] = d.cos(w.widen1(a))
    def name = "Cos"
    private[patterns] def aux: List[Aux] = w :: d :: Nil
  }

  final case class Tan[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A): C[Double] = d.tan(w.widen1(a))
    def name = "Tan"
    private[patterns] def aux: List[Aux] = w :: d :: Nil
  }

  final case class Asin[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A): C[Double] = d.asin(w.widen1(a))
    def name = "Asin"
    private[patterns] def aux: List[Aux] = w :: d :: Nil
  }

  final case class Acos[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A): C[Double] = d.acos(w.widen1(a))
    def name = "Acos"
    private[patterns] def aux: List[Aux] = w :: d :: Nil
  }

  final case class Atan[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A): C[Double] = d.atan(w.widen1(a))
    def name = "Atan"
    private[patterns] def aux: List[Aux] = w :: d :: Nil
  }

  final case class Sinh[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A)           : C[Double] = d.sinh(w.widen1(a))
    def name                  : String    = "Sinh"
    private[patterns] def aux : List[Aux] = w :: d :: Nil
  }

  final case class Cosh[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A)           : C[Double] = d.cosh(w.widen1(a))
    def name                  : String    = "Cosh"
    private[patterns] def aux : List[Aux] = w :: d :: Nil
  }

  final case class Tanh[C[_], A]()(implicit w: Widen[A, C[Double]], d: NumDoubleC[C]) extends PureOp[A, C[Double]] {
    def apply(a: A)           : C[Double] = d.tanh(w.widen1(a))
    def name                  : String    = "Tanh"
    private[patterns] def aux : List[Aux] = w :: d :: Nil
  }

  final case class Rand[A]()(implicit num: Num[A]) extends RandomOp[A, A] {
    def next[Tx](a: A)(implicit state: Random[Tx], tx: Tx): A = num.rand(a)

    def name = "Rand"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  final case class Rand2[A]()(implicit num: Num[A]) extends RandomOp[A, A] {
    def next[Tx](a: A)(implicit state: Random[Tx], tx: Tx): A = num.rand2(a)

    def name = "Rand2"

    private[patterns] def aux: List[Aux] = num :: Nil
  }

  // XXX TODO:
  // Linrand
  // Bilinrand
  // Sum3rand
  // Coin
}

final case class UnaryOp[A1, A](op: UnaryOp.Op[A1, A], a: Pat[A1])
  extends Pattern[A] { pat =>

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl[Tx](tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val aT = t(a)
    if (aT.eq(a)) this else copy(a = aT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val aStream = a.expand(ctx, tx0)

    private[this] implicit val state: op.State[Tx]  = op.prepare(pat.ref)(ctx, tx0)

    def reset()(implicit tx: Tx): Unit =
      aStream.reset()

    def hasNext(implicit tx: Tx): Boolean =
      aStream.hasNext

    def next()(implicit tx: Tx): A = {
      val aVal = aStream.next()
      op.next(aVal)
    }
  }
}

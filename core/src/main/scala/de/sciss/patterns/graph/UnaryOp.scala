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

import de.sciss.lucre.stm.{Base, TxnRandom}
import de.sciss.patterns.Types.{Aux, Num, NumBool, NumDouble, NumFrac, NumInt, ToNum, Widen, WidenToDouble}
import de.sciss.patterns.stream.UnaryOpImpl
import de.sciss.serial.{DataInput, DataOutput}

import scala.language.higherKinds

object UnaryOp {
  sealed abstract class Op[A1, A2] extends ProductWithAux {
    type State[S <: Base[S]]
    
//    protected def opId: Int
    
    def readState   [S <: Base[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): State[S]
    def writeState  [S <: Base[S]](s: State[S], out: DataOutput): Unit
    def disposeState[S <: Base[S]](s: State[S])(implicit tx: S#Tx): Unit

    def prepare[S <: Base[S]](ref: AnyRef)(implicit ctx: Context[S], tx: S#Tx): State[S]

    def next[S <: Base[S]](a: A1)(implicit state: State[S], tx: S#Tx): A2

    override final def productPrefix = s"UnaryOp$$$name"

    def name: String

    // $COVERAGE-OFF$
    override def toString: String = name
    // $COVERAGE-ON$
  }

  abstract class PureOp[A1, A2] extends Op[A1, A2] {
    final type State[S <: Base[S]] = Unit

    final def readState   [S <: Base[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): State[S] = ()
    final def writeState  [S <: Base[S]](s: State[S], out: DataOutput): Unit = ()
    final def disposeState[S <: Base[S]](s: State[S])(implicit tx: S#Tx): Unit = ()

    final def prepare[S <: Base[S]](ref: AnyRef)(implicit ctx: Context[S], tx: S#Tx): State[S] = ()

    def next[S <: Base[S]](a: A1)(implicit state: State[S], tx: S#Tx): A2 = apply(a)

    def apply(a: A1): A2
  }

  abstract class RandomOp[A1, A2] extends Op[A1, A2] {
    final type State[S <: Base[S]] = TxnRandom[S]

    final def readState[S <: Base[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): State[S] =
      TxnRandom.read(in, access)
    
    final def writeState[S <: Base[S]](s: State[S], out: DataOutput): Unit =
      s.write(out)

    final def disposeState[S <: Base[S]](s: State[S])(implicit tx: S#Tx): Unit =
      s.dispose()

    final def prepare[S <: Base[S]](ref: AnyRef)(implicit ctx: Context[S], tx: S#Tx): State[S] = ctx.mkRandom(ref)
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

  final case class BitNot[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.unary_~(a)
    def name                  : String    = "BitNot"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Abs[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.abs(a)
    def name                  : String    = "Abs"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class ToDouble[A, B]()(implicit to: ToNum[A] { type Double = B }) extends PureOp[A, B] {
    def apply(a: A)           : B         = to.toDouble(a)
    def name                  : String    = "ToDouble"
    private[patterns] def aux : List[Aux] = to :: Nil
  }

  final case class ToInt[A, B]()(implicit to: ToNum[A] { type Int = B }) extends PureOp[A, B] {
    def apply(a: A)           : B         = to.toInt(a)
    def name                  : String    = "ToInt"
    private[patterns] def aux : List[Aux] = to :: Nil
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

  final case class Sqrt[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.sqrt(wd.widen1(a))
    def name                  : String    = "Sqrt"
    private[patterns] def aux : List[Aux] = wd :: Nil
  }

  final case class Exp[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.exp(wd.widen1(a))
    def name                  : String    = "Exp"
    private[patterns] def aux : List[Aux] = wd :: Nil
  }

  final case class Reciprocal[A, B]()(implicit w: Widen[A, B], num: NumFrac[B]) extends PureOp[A, B] {
    def apply(a: A )          : B         = num.reciprocal(w.widen1(a))
    def name                  : String    = "Reciprocal"
    private[patterns] def aux : List[Aux] = w :: num :: Nil
  }

  final case class Midicps[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.midiCps(wd.widen1(a))
    def name                  : String    = "Midicps"
    private[patterns] def aux : List[Aux] = wd :: Nil
  }

  final case class Cpsmidi[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.cpsMidi(wd.widen1(a))
    def name                  : String    = "Cpsmidi"
    private[patterns] def aux : List[Aux] = wd :: Nil
  }

  final case class Midiratio[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.midiRatio(wd.widen1(a))
    def name                  : String    = "Midiratio"
    private[patterns] def aux : List[Aux] = wd :: Nil
  }

  final case class Ratiomidi[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.ratioMidi(wd.widen1(a))
    def name                  : String    = "Ratiomidi"
    private[patterns] def aux : List[Aux] = wd :: Nil
  }

  final case class Dbamp[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.dbAmp(wd.widen1(a))
    def name                  : String    = "Dbamp"
    private[patterns] def aux : List[Aux] = wd :: Nil
  }

  final case class Ampdb[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.ampDb(wd.widen1(a))
    def name                  : String    = "Ampdb"
    private[patterns] def aux : List[Aux] = wd :: Nil
  }

  final case class Octcps[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.octCps(wd.widen1(a))
    def name                  : String    = "Octcps"
    private[patterns] def aux : List[Aux] = wd :: Nil
  }

  final case class Cpsoct[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.cpsOct(wd.widen1(a))
    def name                  : String    = "Cpsoct"
    private[patterns] def aux : List[Aux] = wd :: Nil
  }

  final case class Log[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.log(wd.widen1(a))
    def name                  : String    = "Log"
    private[patterns] def aux : List[Aux] = wd :: Nil
  }

  final case class Log2[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A): B         = wd.log2(wd.widen1(a))
    def name = "Log2"
    private[patterns] def aux: List[Aux] = wd :: Nil
  }

  final case class Log10[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A): B         = wd.log10(wd.widen1(a))
    def name = "Log10"
    private[patterns] def aux: List[Aux] = wd :: Nil
  }

  final case class Sin[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A): B         = wd.sin(wd.widen1(a))
    def name = "Sin"
    private[patterns] def aux: List[Aux] = wd :: Nil
  }

  final case class Cos[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A): B         = wd.cos(wd.widen1(a))
    def name = "Cos"
    private[patterns] def aux: List[Aux] = wd :: Nil
  }

  final case class Tan[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A): B         = wd.tan(wd.widen1(a))
    def name = "Tan"
    private[patterns] def aux: List[Aux] = wd :: Nil
  }

  final case class Asin[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A): B         = wd.asin(wd.widen1(a))
    def name = "Asin"
    private[patterns] def aux: List[Aux] = wd :: Nil
  }

  final case class Acos[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A): B         = wd.acos(wd.widen1(a))
    def name = "Acos"
    private[patterns] def aux: List[Aux] = wd :: Nil
  }

  final case class Atan[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A): B         = wd.atan(wd.widen1(a))
    def name = "Atan"
    private[patterns] def aux: List[Aux] = wd :: Nil
  }

  final case class Sinh[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.sinh(wd.widen1(a))
    def name                  : String    = "Sinh"
    private[patterns] def aux : List[Aux] = wd :: Nil
  }

  final case class Cosh[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.cosh(wd.widen1(a))
    def name                  : String    = "Cosh"
    private[patterns] def aux : List[Aux] = wd :: Nil
  }

  final case class Tanh[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.tanh(wd.widen1(a))
    def name                  : String    = "Tanh"
    private[patterns] def aux : List[Aux] = wd :: Nil
  }

  final case class Rand[A]()(implicit num: Num[A]) extends RandomOp[A, A] {
    def next[S <: Base[S]](a: A)(implicit state: TxnRandom[S], tx: S#Tx): A = num.rand(a)
    def name                  : String    = "Rand"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  final case class Rand2[A]()(implicit num: Num[A]) extends RandomOp[A, A] {
    def next[S <: Base[S]](a: A)(implicit state: TxnRandom[S], tx: S#Tx): A = num.rand2(a)
    def name                  : String    = "Rand2"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  // XXX TODO:
  // Linrand
  // Bilinrand
  // Sum3rand

  // Distort
  // Softclip

  final case class Coin[A, B]()(implicit num: NumDouble[A] { type Boolean = B }) extends RandomOp[A, B] {
    def next[S <: Base[S]](a: A)(implicit state: TxnRandom[S], tx: S#Tx): B = num.coin(a)
    def name                  : String    = "Coin"
    private[patterns] def aux : List[Aux] = num :: Nil
  }

  // RectWindow
  // HanWindow
  // WelWindow
  // TriWindow

  // Ramp
  // Scurve
}

final case class UnaryOp[A1, A](op: UnaryOp.Op[A1, A], a: Pat[A1])
  extends Pattern[A] { pat =>

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    UnaryOpImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val aT = t(a)
    if (aT.eq(a)) this else copy(a = aT)
  }
}

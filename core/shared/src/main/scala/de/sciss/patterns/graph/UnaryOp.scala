/*
 *  UnaryOp.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package graph

import de.sciss.lucre.Adjunct.{Num, NumBool, NumDouble, NumFrac, NumInt, ToNum, Widen, WidenToDouble}
import de.sciss.lucre.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.{Exec, RandomObj}
import de.sciss.patterns.stream.UnaryOpImpl
import de.sciss.serial.{DataInput, DataOutput}

object UnaryOp {
//  object Op {
//    type Stated[A1, A2, S[~ <: Exec[~]]] = Op[A1, A2] { type State[T <: Exec[T]] = S[T] }
//  }
  sealed abstract class Op[A1, A2] extends ProductWithAdjuncts {
    type State[T <: Exec[T]]

    def readState   [T <: Exec[T]](in: DataInput)(implicit tx: T): State[T]
    def writeState  [T <: Exec[T]](s: State[T], out: DataOutput): Unit
    def disposeState[T <: Exec[T]](s: State[T])(implicit tx: T): Unit

    def copyState[T <: Exec[T], Out <: Exec[Out]](s: State[T])(implicit tx: T, txOut: Out): State[Out]

    def prepare[T <: Exec[T]](ref: AnyRef)(implicit ctx: Context[T], tx: T): State[T]

    def next[T <: Exec[T]](a: A1)(implicit state: State[T], tx: T): A2

    override final def productPrefix = s"UnaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  abstract class PureOp[A1, A2] extends Op[A1, A2] {
    final type State[T <: Exec[T]] = Unit

    final def readState   [T <: Exec[T]](in: DataInput)(implicit tx: T): State[T] = ()
    final def writeState  [T <: Exec[T]](s: State[T], out: DataOutput): Unit = ()
    final def disposeState[T <: Exec[T]](s: State[T])(implicit tx: T): Unit = ()

    final def copyState[T <: Exec[T], Out <: Exec[Out]](s: State[T])(implicit tx: T, txOut: Out): Unit = ()

    final def prepare[T <: Exec[T]](ref: AnyRef)(implicit ctx: Context[T], tx: T): State[T] = ()

    def next[T <: Exec[T]](a: A1)(implicit state: State[T], tx: T): A2 = apply(a)

    def apply(a: A1): A2
  }

  abstract class RandomOp[A1, A2] extends Op[A1, A2] {
    final type State[T <: Exec[T]] = RandomObj[T]

    final def readState[T <: Exec[T]](in: DataInput)(implicit tx: T): State[T] =
      RandomObj.read(in)
    
    final def writeState[T <: Exec[T]](s: State[T], out: DataOutput): Unit =
      s.write(out)

    final def disposeState[T <: Exec[T]](s: State[T])(implicit tx: T): Unit =
      s.dispose()

    def copyState[T <: Exec[T], Out <: Exec[Out]](s: RandomObj[T])(implicit tx: T, txOut: Out): RandomObj[Out] =
      s.copy[Out]()

    final def prepare[T <: Exec[T]](ref: AnyRef)(implicit ctx: Context[T], tx: T): State[T] = ctx.mkRandom(ref)
  }

  // ---- analogous to UGens ----

  final case class Neg[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.negate(a)
    def name                  : String    = "Neg"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Not[A]()(implicit num: NumBool[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.negate(a)
    def name                  : String    = "Not"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class BitNot[A]()(implicit num: NumInt[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.not(a)
    def name                  : String    = "BitNot"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Abs[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.abs(a)
    def name                  : String    = "Abs"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class ToDouble[A, B]()(implicit to: ToNum[A] { type Double = B }) extends PureOp[A, B] {
    def apply(a: A)           : B         = to.toDouble(a)
    def name                  : String    = "ToDouble"
    override def adjuncts: List[Adjunct] = to :: Nil
  }

  final case class ToInt[A, B]()(implicit to: ToNum[A] { type Int = B }) extends PureOp[A, B] {
    def apply(a: A)           : B         = to.toInt(a)
    def name                  : String    = "ToInt"
    override def adjuncts: List[Adjunct] = to :: Nil
  }

  final case class Ceil[A]()(implicit num: NumFrac[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.ceil(a)
    def name                  : String    = "Ceil"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Floor[A]()(implicit num: NumFrac[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.floor(a)
    def name                  : String    = "Floor"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Frac[A]()(implicit num: NumFrac[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.frac(a)
    def name                  : String    = "Frac"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Signum[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.signum(a)
    def name                  : String    = "Signum"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Squared[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.squared(a)
    def name                  : String    = "Squared"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Cubed[A]()(implicit num: Num[A]) extends PureOp[A, A] {
    def apply(a: A)           : A         = num.cubed(a)
    def name                  : String    = "Cubed"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Sqrt[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.sqrt(wd.widen1(a))
    def name                  : String    = "Sqrt"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Exp[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.exp(wd.widen1(a))
    def name                  : String    = "Exp"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Reciprocal[A, B]()(implicit w: Widen[A, B], num: NumFrac[B]) extends PureOp[A, B] {
    def apply(a: A )          : B         = num.reciprocal(w.widen1(a))
    def name                  : String    = "Reciprocal"
    override def adjuncts: List[Adjunct] = w :: num :: Nil
  }

  final case class Midicps[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.midiCps(wd.widen1(a))
    def name                  : String    = "Midicps"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Cpsmidi[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.cpsMidi(wd.widen1(a))
    def name                  : String    = "Cpsmidi"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Midiratio[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.midiRatio(wd.widen1(a))
    def name                  : String    = "Midiratio"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Ratiomidi[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.ratioMidi(wd.widen1(a))
    def name                  : String    = "Ratiomidi"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Dbamp[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.dbAmp(wd.widen1(a))
    def name                  : String    = "Dbamp"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Ampdb[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.ampDb(wd.widen1(a))
    def name                  : String    = "Ampdb"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Octcps[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.octCps(wd.widen1(a))
    def name                  : String    = "Octcps"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Cpsoct[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.cpsOct(wd.widen1(a))
    def name                  : String    = "Cpsoct"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Log[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.log(wd.widen1(a))
    def name                  : String    = "Log"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Log2[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A): B         = wd.log2(wd.widen1(a))
    def name = "Log2"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Log10[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A): B         = wd.log10(wd.widen1(a))
    def name = "Log10"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Sin[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A): B         = wd.sin(wd.widen1(a))
    def name = "Sin"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Cos[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A): B         = wd.cos(wd.widen1(a))
    def name = "Cos"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Tan[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A): B         = wd.tan(wd.widen1(a))
    def name = "Tan"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Asin[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A): B         = wd.asin(wd.widen1(a))
    def name = "Asin"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Acos[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A): B         = wd.acos(wd.widen1(a))
    def name = "Acos"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Atan[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A): B         = wd.atan(wd.widen1(a))
    def name = "Atan"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Sinh[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.sinh(wd.widen1(a))
    def name                  : String    = "Sinh"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Cosh[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.cosh(wd.widen1(a))
    def name                  : String    = "Cosh"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Tanh[A, B]()(implicit wd: WidenToDouble[A, B]) extends PureOp[A, B] {
    def apply(a: A)           : B         = wd.tanh(wd.widen1(a))
    def name                  : String    = "Tanh"
    override def adjuncts: List[Adjunct] = wd :: Nil
  }

  final case class Rand[A]()(implicit num: Num[A]) extends RandomOp[A, A] {
    def next[T <: Exec[T]](a: A)(implicit state: RandomObj[T], tx: T): A = num.rand(a)
    def name                  : String    = "Rand"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class Rand2[A]()(implicit num: Num[A]) extends RandomOp[A, A] {
    def next[T <: Exec[T]](a: A)(implicit state: RandomObj[T], tx: T): A = num.rand2(a)
    def name                  : String    = "Rand2"
    override def adjuncts: List[Adjunct] = num :: Nil
  }

  // XXX TODO:
  // Linrand
  // Bilinrand
  // Sum3rand

  // Distort
  // Softclip

  final case class Coin[A, B]()(implicit num: NumDouble[A] { type Boolean = B }) extends RandomOp[A, B] {
    def next[T <: Exec[T]](a: A)(implicit state: RandomObj[T], tx: T): B = num.coin(a)
    def name                  : String    = "Coin"
    override def adjuncts: List[Adjunct] = num :: Nil
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

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    UnaryOpImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val aT = t(a)
    if (aT.eq(a)) this else copy(a = aT)
  }
}

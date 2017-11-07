/*
 *  PE.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns

import de.sciss.patterns.PE.Value
import de.sciss.patterns.graph.impl.PESeq
import de.sciss.patterns.graph.{ConstantD, ConstantI, ConstantIs}
import de.sciss.patterns.impl.Util

import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

/** Pattern element */
object PE {
  trait Lazy[+A <: Value] extends Lazy.Expander[Stream[A]] with PE[A]

  implicit def fromInt   (x: scala.Int   ): ConstantI = new ConstantI(x)
  implicit def fromDouble(x: scala.Double): ConstantD = new ConstantD(x)
//  implicit def fromLong  (x: Long  ): ConstantL = new ConstantL(x)

  implicit def fromIntSeq(xs: Seq[scala.Int]): ConstantIs = ConstantIs(xs.toIndexedSeq) // PESeq[Int] = PESeq(xs.map(ConstantI(_))(breakOut))

  implicit def fromSeq[A <: Value](xs: scala.Seq[PE[A]]): PE[A] = xs match {
    case scala.Seq(x) => x
    case _            => PESeq(xs.toIndexedSeq)
  }

  sealed trait Value {
    type Out
  }
  sealed trait Numeric extends Value {
    val num: scala.Numeric[Out]
  }
  object Value {
    sealed trait Int_* extends Numeric
    object Int extends Int
    sealed abstract class Int extends Int_* {
      final type Out = scala.Int
      final val num: scala.Numeric[scala.Int] = implicitly[scala.Numeric[scala.Int]]
    }
    object IntSeq extends IntSeq
    sealed abstract class IntSeq extends Int_* {
      final type Out = Vec[scala.Int]
      final val num: scala.Numeric[Vec[scala.Int]] = Util.vecNumeric
    }

    sealed trait Double_* extends Numeric
    object Double extends Double
    sealed abstract class Double extends Double_* {
      final type Out = scala.Double
      final val num: scala.Numeric[scala.Double] = implicitly[scala.Numeric[scala.Double]]
    }
    object DoubleSeq extends DoubleSeq
    sealed trait DoubleSeq extends Double_* {
      final type Out = Vec[scala.Double]
      final val num: scala.Numeric[Vec[scala.Double]] = ???
    }
  }

  type Int      = PE[Value.Int     ]
  type Int_*    = PE[Value.Int_*   ]
  type Double   = PE[Value.Double  ]
  type Double_* = PE[Value.Double_*]
}
trait PE[+A <: Value] extends Product {
  private[patterns] def expand(implicit b: StreamGraph.Builder): Stream[A] // StreamInLike

  def toStream(implicit b: StreamGraph.Builder): Stream[A] // InLike
}
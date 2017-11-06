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

import de.sciss.patterns.graph.{ConstantD, ConstantI, ConstantL}
import de.sciss.patterns.graph.impl.PESeq

import scala.language.implicitConversions

/** Pattern element */
object PE {
  trait Lazy extends Lazy.Expander[StreamInLike] with PE

  implicit def fromInt   (x: Int   ): ConstantI = new ConstantI(x)
  implicit def fromDouble(x: Double): ConstantD = new ConstantD(x)
  implicit def fromLong  (x: Long  ): ConstantL = new ConstantL(x)

  implicit def fromSeq(xs: scala.Seq[PE]): PE = xs match {
    case scala.Seq(x) => x
    case _            => PESeq(xs.toIndexedSeq)
  }
}
trait PE extends Product {
  private[patterns] def expand(implicit b: StreamGraph.Builder): StreamInLike
}
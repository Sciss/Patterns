/*
 *  Sum.scala
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

import de.sciss.patterns.Types.{Aux, Num}

final case class Sum[A](in: Pat[A])(implicit num: Num[A]) extends Pattern[A] {
  override private[patterns] def aux: List[Aux] = num :: Nil

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl[Tx](tx)

  def transform(t: Transform): Pat[A] = {
    val inT = t(in) .transform(t)
    if (inT.eq(in)) this else copy(in = inT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    def reset()(implicit tx: Tx): Unit = {
      println("Sum. TODO: reset")
      ???
    }

    def hasNext(implicit tx: Tx): Boolean = {
      println("Sum. TODO: hasNext")
      ???
    }

    def next()(implicit tx: Tx): A = {
      println("Sum. TODO: next")
      ???
    }
  }
}
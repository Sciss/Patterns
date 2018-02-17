/*
 *  Differentiate.scala
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
import de.sciss.patterns.{Context, Pat, Pattern, Stream}

final case class Differentiate[A](in: Pat[A])(implicit num: Num[A]) extends Pattern[A] {
  override private[patterns] def aux: List[Aux] = num :: Nil

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl[Tx](tx)

  def transform(t: Transform): Pat[A] = {
    val inT = t(in)
    if (inT eq in) this else copy(in = inT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    def reset()(implicit tx: Tx): Unit = ???

    def hasNext(implicit tx: Tx): Boolean = ???

    def next()(implicit tx: Tx): A = ???
  }
}
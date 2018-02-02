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

package de.sciss.patterns.graph

import de.sciss.patterns.{Context, Pat, Pattern, Stream}
import de.sciss.patterns.Types.{Aux, Num, Top}

final case class Differentiate[T <: Top](in: Pat[T])(implicit num: Num[T]) extends Pattern[T] {
  override private[patterns] def aux: List[Aux] = num :: Nil

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    def reset()(implicit tx: Tx): Unit = ???

    def hasNext(implicit tx: Tx): Boolean = ???

    def next()(implicit tx: Tx): T#Out[Tx] = ???
  }
}
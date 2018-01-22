/*
 *  Indices.scala
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

import de.sciss.patterns.Types.{IntTop, Top}

case class Indices[T <: Top](in: Pat[T]) extends Pattern[IntTop] {
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, Int] = new StreamImpl(tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, Int] {
    private[this] val inStream = in.expand(ctx, tx0)

    private[this] val count = ctx.newVar(0)

    def reset()(implicit tx: Tx): Unit =
      count() = 0

    def hasNext(implicit tx: Tx): Boolean = inStream.hasNext

    def next()(implicit tx: Tx): Int = {
      val res = count()
      inStream.next()
      count() = res + 1
      res
    }
  }
}

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

case class Indices[A](in: Pat[A]) extends Pattern[Int] {
  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, Int] = new StreamImpl(tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[Int] = {
    val inT = t(in)
    if (inT eq in) this else copy(in = inT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, Int] {
    private[this] val id        = ctx.newID()(tx0)
    private[this] val inStream  = in.expand(ctx, tx0)
    private[this] val count     = ctx.newIntVar(id, 0)(tx0)

    def reset()(implicit tx: Tx): Unit = {
      inStream.reset()
      count() = 0
    }

    def hasNext(implicit tx: Tx): Boolean =
      inStream.hasNext

    def next()(implicit tx: Tx): Int = {
      val res = count()
      inStream.next()
      count() = res + 1
      res
    }
  }
}

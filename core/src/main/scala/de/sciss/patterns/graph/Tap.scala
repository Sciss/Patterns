/*
 *  Tap.scala
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

final case class Tap[A, A1](in: Pat[A], side: Pat[A1]) extends Pattern[A] {
  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl[Tx](tx)

  def transform(t: Transform): Pat[A] = {
    val inT   = t(in)
    val sideT = t(side)
    if (inT.eq(in) && sideT.eq(side)) this else copy(in = inT, side = sideT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val inStream    = in  .expand(ctx, tx0)
    private[this] val sideStream  = side.expand(ctx, tx0)

    def reset()(implicit tx: Tx): Unit = {
      inStream  .reset()
      sideStream.reset()
    }

    def hasNext(implicit tx: Tx): Boolean =
      inStream.hasNext

    def next()(implicit tx: Tx): A = {
      val res = inStream.next()
      if (sideStream.hasNext) sideStream.next()
      res
    }
  }
}
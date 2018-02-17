/*
 *  Recur.scala
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

final case class Recur[A](in: Pat[A]) extends Pattern[A] {
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = {
    logStream(s"Copy($in).iterator")
    new StreamImpl[Tx](tx)
  }

  def transform(t: Transform): Pat[A] = {
    val inT = t(in)
    if (inT.eq(in)) this else copy(in = inT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val peer = in.expand(ctx, tx0)

    def reset()(implicit tx: Tx): Unit = {
      logStream(s"Copy($in).iterator.reset()")
      peer.reset()
    }

    def hasNext(implicit tx: Tx): Boolean =
      peer.hasNext

    def next()(implicit tx: Tx): A = {
      val res = peer.next()
      logStream(s"Copy($in).iterator.next() = $res")
      res
    }
  }
}

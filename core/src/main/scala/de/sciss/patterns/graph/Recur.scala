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

import de.sciss.patterns.Types.Top

final case class Recur[T <: Top](in: Pat[T]) extends Pattern[T] {
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = {
    logStream(s"Copy($in).iterator")
    new StreamImpl[Tx](tx)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    private[this] val peer = in.expand(ctx, tx0)

    def reset()(implicit tx: Tx): Unit = {
      logStream(s"Copy($in).iterator.reset()")
      peer.reset()
    }

    def hasNext(implicit tx: Tx): Boolean   = peer.hasNext
    def next()(implicit tx: Tx): T#Out[Tx]  = {
      val res = peer.next()
      logStream(s"Copy($in).iterator.next() = $res")
      res
    }
  }
}

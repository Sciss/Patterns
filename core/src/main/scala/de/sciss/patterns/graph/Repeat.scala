/*
 *  Repeat.scala
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

final case class Repeat[T <: Top](in: Pat[T], times: Pat.Int = Int.MaxValue) extends Pattern[T] {
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = new StreamImpl(tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    private[this] val inStream    = in    .expand(ctx, tx0)
    private[this] val timesStream = times .expand(ctx, tx0)

    private[this] val count       = ctx.newVar(0)
    private[this] val timesVal    = ctx.newVar(0)
    private[this] val _valid      = ctx.newVar(false)
    private[this] val _hasNext    = ctx.newVar(false)

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()    = true
        val hn      = timesStream.hasNext
        count()     = 0
        _hasNext()  = hn
        if (hn) {
          val tv      = timesStream.next()
          timesVal()  = tv
          _hasNext()  = tv > 0 && inStream.hasNext
        }
      }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): T#Out[Tx] = {
      if (!hasNext) Stream.exhausted()
      val res = inStream.next()
      if (!inStream.hasNext) {
        val c   = count() + 1
        count() = c
        val hn  = (c < timesVal()) && {
          inStream.reset()
          inStream.hasNext
        }
        _hasNext() = hn
      }
      res
    }
  }
}

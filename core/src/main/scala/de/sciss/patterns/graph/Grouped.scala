/*
 *  Grouped.scala
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

final case class Grouped[T <: Top](in: Pat[T], size: Pat.Int) extends Pattern[Pat[T]] { pat =>
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, Stream[Tx, T#Out[Tx]]] = new StreamImpl(tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, Stream[Tx, T#Out[Tx]]] {
    private[this] val inStream  : Stream[Tx, T#Out[Tx]] = pat.in  .expand(ctx, tx0)
    private[this] val sizeStream: Stream[Tx, Int]       = pat.size.expand(ctx, tx0)

    private[this] val innerStream = ctx.newVar[Stream[Tx, T#Out[Tx]]](null)
    private[this] val _hasNext    = ctx.newVar(false)

    private[this] val _valid      = ctx.newVar(false)

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid() = true
        advance()
      }

    private final class InnerStream(sizeVal: Int) extends Stream[Tx, T#Out[Tx]] {
      private[this] val count = ctx.newVar(0)

      def reset()(implicit tx: Tx): Unit =
        count() = 0

      def hasNext(implicit tx: Tx): Boolean =
        count() < sizeVal && inStream.hasNext

      def next()(implicit tx: Tx): T#Out[Tx] = {
        val i = count()
        if (i >= sizeVal) Stream.exhausted()
        val res = inStream.next()
        count() = i + 1
        res
      }
    }

    private def advance()(implicit tx: Tx): Unit = {
      _hasNext() = sizeStream.hasNext
      if (_hasNext()) {
        val sizeVal = sizeStream.next()
        val inner   = new InnerStream(sizeVal)
        _hasNext()  = inner.hasNext
      }
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): Stream[Tx, T#Out[Tx]] = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = innerStream()
      reset() // advance()
      res
    }
  }
}

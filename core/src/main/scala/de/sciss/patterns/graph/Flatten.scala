/*
 *  PatPat.scala
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

import scala.annotation.tailrec

final case class Flatten[T <: Top](in: Pat[Pat[T]]) extends Pattern[T] {
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = new StreamImpl(tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    private[this] val inStream: Stream[Tx, Stream[Tx, T#Out[Tx]]] = in.expand(ctx, tx0)

    private[this] val hasInner    = ctx.newVar(false)
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

    @tailrec
    private def advance()(implicit tx: Tx): Unit = {
      if (hasInner()) {
        hasInner() = innerStream().hasNext
      }

      _hasNext() = hasInner()
      if (!_hasNext()) {
        _hasNext() = inStream.hasNext
        if (_hasNext()) {
          innerStream() = inStream.next() // .expand
          hasInner() = true
          advance()
        }
      }
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): T#Out[Tx] = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = innerStream().next()
      advance()
      res
    }
  }
}

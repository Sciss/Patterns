/*
 *  Distinct.scala
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

final case class Distinct[T <: Top](in: Pat[T]) extends Pattern[T] {
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    private[this] val inStream = in.expand(ctx, tx0)

    private[this] val seen     = ctx.newVar[Set[T#Out[Tx]]](null)
    private[this] val _hasNext = ctx.newVar[Boolean       ](false)
    private[this] val _next    = ctx.newVar[T#Out[Tx]     ](null.asInstanceOf[T#Out[Tx]])

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

    @tailrec
    private def advance()(implicit tx: Tx): Unit = {
      _hasNext() = inStream.hasNext
      if (_hasNext()) {
        _next()     = inStream.next()
        _hasNext()  = !seen().contains(_next())
        if (_hasNext()) {
          seen() = seen() + _next()
        } else {
          advance()
        }
      }
    }

    private[this] val _valid = ctx.newVar(false)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid() = true
        seen() = Set.empty
        advance()
      }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): T#Out[Tx] = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = _next()
      advance()
      res
    }
  }
}
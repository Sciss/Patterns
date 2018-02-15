/*
 *  Flatten.scala
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

import scala.annotation.tailrec

final case class Flatten[A](in: Pat[Pat[A]]) extends Pattern[A] {
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl(tx)

  def transform(t: Transform): Pat[A] = ???

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val inStream: Stream[Tx, Pat[A]] = in.expand(ctx, tx0)

    private[this] val hasInner    = ctx.newVar(false)
    private[this] val innerStream = ctx.newVar[Stream[Tx, A]](null)
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
          val inPat     = inStream.next()
          innerStream() = inPat.expand
          hasInner() = true
          advance()
        }
      }
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): A = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = innerStream().next()
      advance()
      res
    }
  }
}

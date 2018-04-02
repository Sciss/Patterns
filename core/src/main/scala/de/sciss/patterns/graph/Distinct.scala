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

import scala.annotation.tailrec

final case class Distinct[A](in: Pat[A]) extends Pattern[A] {
  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl[Tx](tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val inT = t(in)
    if (inT eq in) this else copy(in = inT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val inStream = in.expand(ctx, tx0)

    private[this] val seen     = ctx.newVar[Set[A]  ](null)(tx0)
    private[this] val _hasNext = ctx.newVar[Boolean ](false)(tx0)
    private[this] val _next    = ctx.newVar[A       ](null.asInstanceOf[A])(tx0)

    def reset()(implicit tx: Tx): Unit = if (_valid()) {
      _valid() = false
      inStream.reset()
    }

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

    private[this] val _valid = ctx.newVar(false)(tx0)

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

    def next()(implicit tx: Tx): A = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = _next()
      advance()
      res
    }
  }
}
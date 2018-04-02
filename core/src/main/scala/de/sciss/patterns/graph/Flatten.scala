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

import de.sciss.patterns.Context.Var

import scala.annotation.tailrec

final case class Flatten[A](in: Pat[Pat[A]]) extends Pattern[A] {
  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl(tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val inT = t(in)
    if (inT.eq(in)) this else copy(in = inT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val inStream: Stream[Tx, Pat[A]] = in.expand(ctx, tx0)

    private[this] val hasInner    = ctx.newBooleanVar(false)(tx0)
    private[this] val innerStream = ??? : Var[Tx, Stream[Tx, A]] // ctx.newVar[Stream[Tx, A]](null)(tx0)
    private[this] val _hasNext    = ctx.newBooleanVar(false)(tx0)
    private[this] val _valid      = ctx.newBooleanVar(false)(tx0)

    def reset()(implicit tx: Tx): Unit = if (_valid()) {
      _valid() = false
      inStream.reset()
    }

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()    = true
        hasInner()  = false
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
      if (!hasNext) Stream.exhausted()
      val res = innerStream().next()
      advance()
      res
    }
  }
}

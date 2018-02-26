/*
 *  Gate.scala
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

final case class Gate[A](in: Pat[A], gate: Pat[Boolean]) extends Pattern[A] {
  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] =
    new StreamImpl[Tx](tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val inT   = t(in)
    val gateT = t(gate)
    if (inT.eq(in) && gateT.eq(gate)) this else copy(in = inT, gate = gateT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val inStream    = in  .expand(ctx, tx0)
    private[this] val gateStream  = gate.expand(ctx, tx0)

    private[this] val _valid      = ctx.newVar(false)
    private[this] val _hasNext    = ctx.newVar(false)
    private[this] val _nextElem   = ctx.newVar[A](null.asInstanceOf[A])

    def reset()(implicit tx: Tx): Unit = if (_valid()) {
      _valid() = false
      inStream  .reset()
      gateStream.reset()
    }

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid() = true
        advance()
      }

    @tailrec
    private def advance()(implicit tx: Tx): Unit = {
      if (inStream.hasNext) {
        val inVal = inStream.next()
        if (gateStream.hasNext) {
          val gateVal = gateStream.next()
          if (gateVal) {
            _nextElem() = inVal
            _hasNext()  = true
          } else {
            advance()
          }
        } else {
          _hasNext() = false
        }
      } else {
        _hasNext() = false
      }
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = _nextElem()
      advance()
      res
    }
  }
}

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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.impl.PatElem

import scala.annotation.tailrec

final case class Gate[A](in: Pat[A], gate: Pat[Boolean]) extends Pattern[A] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT   = t(in)
    val gateT = t(gate)
    if (inT.eq(in) && gateT.eq(gate)) this else copy(in = inT, gate = gateT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, A] {
    private[this] val id          = tx0.newId()
    private[this] val inStream    = in  .expand(ctx, tx0)
    private[this] val gateStream  = gate.expand(ctx, tx0)
    private[this] val _valid      = tx0.newBooleanVar(id, false)
    private[this] val _hasNext    = tx0.newBooleanVar(id, false)
    private[this] val _nextElem   = PatElem.makeVar[S, A](id)(tx0)

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      inStream  .reset()
      gateStream.reset()
    }

    private def validate()(implicit tx: S#Tx): Unit =
      if (!_valid()) {
        _valid() = true
        advance()
      }

    @tailrec
    private def advance()(implicit tx: S#Tx): Unit = {
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

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = _nextElem()
      advance()
      res
    }
  }
}

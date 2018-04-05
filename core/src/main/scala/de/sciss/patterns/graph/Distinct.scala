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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.impl.PatElem

import scala.annotation.tailrec

final case class Distinct[A](in: Pat[A]) extends Pattern[A] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT = t(in)
    if (inT eq in) this else copy(in = inT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, A] {
    private[this] val id        = tx0.newId()
    private[this] val inStream  = in.expand(ctx, tx0)

    private[this] val seen      = tx0.newVar[Set[A]](id, Set.empty)(PatElem.setSerializer)
    private[this] val _hasNext  = tx0.newBooleanVar(id, false)
    private[this] val _next     = PatElem.makeVar[S, A](id)(tx0)
    private[this] val _valid    = tx0.newBooleanVar(id, false)

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      inStream.reset()
    }

    @tailrec
    private def advance()(implicit tx: S#Tx): Unit = {
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

    private def validate()(implicit tx: S#Tx): Unit =
      if (!_valid()) {
        _valid() = true
        seen() = Set.empty
        advance()
      }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = _next()
      advance()
      res
    }
  }
}
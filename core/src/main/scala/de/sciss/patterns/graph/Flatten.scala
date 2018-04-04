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

import de.sciss.lucre.stm.Base

import scala.annotation.tailrec

final case class Flatten[A](in: Pat[Pat[A]]) extends Pattern[A] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new StreamImpl(tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT = t(in)
    if (inT.eq(in)) this else copy(in = inT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, A] {
    private[this] val id          = tx0.newId()
    private[this] val inStream    = in.expand(ctx, tx0)
    private[this] val hasInner    = tx0.newBooleanVar(id, false)
    private[this] val innerStream = ??? : S#Var[Stream[S, A]] // ctx.newVar[Stream[S, A]](null)(tx0)
    private[this] val _hasNext    = tx0.newBooleanVar(id, false)
    private[this] val _valid      = tx0.newBooleanVar(id, false)

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      inStream.reset()
    }

    private def validate()(implicit tx: S#Tx): Unit =
      if (!_valid()) {
        _valid()    = true
        hasInner()  = false
        advance()
      }

    @tailrec
    private def advance()(implicit tx: S#Tx): Unit = {
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

    def hasNext(implicit tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = innerStream().next()
      advance()
      res
    }
  }
}

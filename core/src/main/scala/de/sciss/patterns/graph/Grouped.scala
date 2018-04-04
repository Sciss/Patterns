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

import de.sciss.lucre.stm.Base

final case class Grouped[A](in: Pat[A], size: Pat[Int]) extends Pattern[Pat[A]] { pat =>
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] = new StreamImpl(tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Pat[A]] = {
    val inT   = t(in)
    val sizeT = t(size)
    if (inT.eq(in) && sizeT.eq(size)) this else copy(in = inT, size = sizeT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, Pat[A]] {
    private[this] val id          = tx0.newId()
    private[this] val inStream    = pat.in  .expand(ctx, tx0)
    private[this] val sizeStream  = pat.size.expand(ctx, tx0)
    private[this] val innerStream = ??? : S#Var[Pat[A]] // ctx.newVar[Pat[A]](null)(tx0) // Stream[S, A]](null)
    private[this] val _hasNext    = tx0.newBooleanVar(id, false)
    private[this] val _valid      = tx0.newBooleanVar(id, false)

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      inStream  .reset()
      sizeStream.reset()
    }

    private def validate()(implicit tx: S#Tx): Unit =
      if (!_valid()) {
        _valid() = true
        advance()
      }

    private def advance()(implicit tx: S#Tx): Unit = {
      _hasNext() = sizeStream.hasNext && inStream.hasNext
      if (_hasNext()) {
        val sizeVal = math.max(0, sizeStream.next())
        val b       = Vector.newBuilder[A]
        b.sizeHint(sizeVal)
        var i = 0
        // there is _no_ reasonable way to provide the
        // stream than to eagerly collect the values here,
        // because of the order of execution between inner and outer
        // `next`!
        while (i < sizeVal && inStream.hasNext) {
          b += inStream.next()
          i += 1
        }
        val inner     = Pat(b.result: _*) // Stream[S, A](b.result: _*)
        innerStream() = inner
        _hasNext()    = sizeVal > 0
      }
    }

    def hasNext(implicit tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: S#Tx): Pat[A] = {
      if (!hasNext) Stream.exhausted()
      val res = innerStream()
      advance()
      res
    }
  }
}

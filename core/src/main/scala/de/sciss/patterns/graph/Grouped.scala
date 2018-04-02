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

final case class Grouped[A](in: Pat[A], size: Pat[Int]) extends Pattern[Pat[A]] { pat =>
  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, Pat[A]] = new StreamImpl(tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[Pat[A]] = {
    val inT   = t(in)
    val sizeT = t(size)
    if (inT.eq(in) && sizeT.eq(size)) this else copy(in = inT, size = sizeT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, Pat[A]] {
    private[this] val inStream  : Stream[Tx, A]   = pat.in  .expand(ctx, tx0)
    private[this] val sizeStream: Stream[Tx, Int] = pat.size.expand(ctx, tx0)

    private[this] val innerStream = ctx.newVar[Pat[A]](null)(tx0) // Stream[Tx, A]](null)
    private[this] val _hasNext    = ctx.newVar(false)(tx0)
    private[this] val _valid      = ctx.newVar(false)(tx0)

    def reset()(implicit tx: Tx): Unit = if (_valid()) {
      _valid() = false
      inStream  .reset()
      sizeStream.reset()
    }

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid() = true
        advance()
      }

    private def advance()(implicit tx: Tx): Unit = {
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
        val inner     = Pat(b.result: _*) // Stream[Tx, A](b.result: _*)
        innerStream() = inner
        _hasNext()    = sizeVal > 0
      }
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): Pat[A] = {
      if (!hasNext) Stream.exhausted()
      val res = innerStream()
      advance()
      res
    }
  }
}

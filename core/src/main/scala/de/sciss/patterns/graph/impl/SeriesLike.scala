/*
 *  SeriesLike.scala
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
package impl

import de.sciss.patterns.Types.{Bridge, Top}

trait SeriesLike[T1 <: Top, T2 <: Top, T <: Top] extends Pattern[T] {
  // ---- abstract ----

  def start: Pat[T1]

  protected val br: Bridge[T1, T2, T]

  protected def step: Pat[T2]

  protected def op[Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx]

  // ---- impl ----

  final def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = new StreamImpl(tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    private[this] val ai = start.expand(ctx, tx0).map(br.lift1)
    private[this] val bi = step .expand(ctx, tx0).map(br.lift2)

    private[this] val state     = ctx.newVar[T#Out[Tx]](null.asInstanceOf[T#Out[Tx]])
    private[this] val _hasNext  = ctx.newVar(false)
    private[this] val _valid    = ctx.newVar(false)

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid() = true
        _hasNext() = ai.hasNext
        if (_hasNext()) state() = ai.next()
      }

    def next()(implicit tx: Tx): T#Out[Tx] = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = state()
      _hasNext() = /* ai.hasNext && */ bi.hasNext
      if (_hasNext()) {
        state() = op(res, bi.next())
      }
      res
    }
  }
}

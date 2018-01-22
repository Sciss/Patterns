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

  protected def op[Tx](a: T#TOut[Tx], b: T#TOut[Tx]): T#TOut[Tx]

  // ---- impl ----

  final def iterator[Tx](implicit ctx: Context[Tx]): Stream[Tx, T#TOut[Tx]] = new Stream[Tx, T#TOut[Tx]] {
    private[this] val ai = start.expand.map(br.lift1)
    private[this] val bi = step .expand.map(br.lift2)

    private[this] val state     = ctx.newVar[T#TOut[Tx]](null.asInstanceOf[T#TOut[Tx]])
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

    def next()(implicit tx: Tx): T#TOut[Tx] = {
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

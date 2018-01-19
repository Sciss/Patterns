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

  protected def op(a: T#Out, b: T#Out): T#Out

  // ---- impl ----

  final def iterator(implicit ctx: Context): Stream[T#Out] = new Stream[T#Out] {
    private[this] val ai = start.expand.map(br.lift1)
    private[this] val bi = step .expand.map(br.lift2)

    private[this] var state   : T#Out   = _
    private[this] var _hasNext: Boolean = _

    def hasNext: Boolean = _hasNext

    def reset(): Unit = {
      _hasNext = ai.hasNext
      if (_hasNext) state = ai.next()
    }

    reset()

    def next(): T#Out = {
      if (!_hasNext) Stream.exhausted()
      val res = state
      _hasNext = /* ai.hasNext && */ bi.hasNext
      if (_hasNext) {
        state = op(state, bi.next())
      }
      res
    }
  }
}

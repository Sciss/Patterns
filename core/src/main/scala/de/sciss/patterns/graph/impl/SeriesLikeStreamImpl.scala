/*
 *  SeriesLikeStreamImpl.scala
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

import de.sciss.patterns.Context.Var
import de.sciss.patterns.Types.Widen2

abstract class SeriesLikeStreamImpl[A1, A2, A, Tx](start: Pat[A1], step: Pat[A2], tx0: Tx)
                                                  (implicit ctx: Context[Tx], w: Widen2[A1, A2, A])
  extends Stream[Tx, A] {

  protected def op(a: A, b: A): A

  private[this] val id          = ctx.newID()(tx0)
  private[this] val startStream = start .expand(ctx, tx0).map(w.widen1)(ctx, tx0)
  private[this] val stepStream  = step  .expand(ctx, tx0).map(w.widen2)(ctx, tx0)
  private[this] val state       = ??? : Var[Tx, A] // ctx.newVar[A](null.asInstanceOf[A])(tx0)
  private[this] val _hasNext    = ctx.newBooleanVar(id, false)(tx0)
  private[this] val _valid      = ctx.newBooleanVar(id, false)(tx0)

  final def hasNext(implicit tx: Tx): Boolean = {
    validate()
    _hasNext()
  }

  final def reset()(implicit tx: Tx): Unit = if (_valid()) {
    _valid() = false
    startStream .reset()
    stepStream  .reset()
  }

  private def validate()(implicit tx: Tx): Unit =
    if (!_valid()) {
      _valid()    = true
//      count()     = 0
      _hasNext()  = startStream.hasNext // && lengthStream.hasNext
      if (_hasNext()) {
        state()     = startStream .next()
//        lengthVal() = lengthStream.next()
      }
    }

  final def next()(implicit tx: Tx): A = {
    if (!hasNext) Stream.exhausted()
    val res = state()
//    val c   = count() + 1
//    count() = c
    _hasNext() = stepStream.hasNext // && c < lengthVal()
    if (_hasNext()) {
      state() = op(res, stepStream.next())
    }
    res
  }
}
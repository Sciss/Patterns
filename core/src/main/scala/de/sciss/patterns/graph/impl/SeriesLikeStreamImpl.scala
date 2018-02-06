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

import de.sciss.patterns.Types.{Widen, Top}

abstract class SeriesLikeStreamImpl[T1 <: Top, T2 <: Top, T <: Top, Tx](start: Pat[T1], step: Pat[T2], // length: Pat.Int,
                                                                        tx0: Tx)
                                                                       (implicit ctx: Context[Tx], widen: Widen[T1, T2, T])
  extends Stream[Tx, T#Out[Tx]] {

  protected def op(a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx]

  private[this] val startStream   = start .expand(ctx, tx0).map(widen.lift1)
  private[this] val stepStream    = step  .expand(ctx, tx0).map(widen.lift2)
//  private[this] val lengthStream  = length.expand(ctx, tx0)

  private[this] val state     = ctx.newVar[T#Out[Tx]](null.asInstanceOf[T#Out[Tx]])
//  private[this] val lengthVal = ctx.newVar(0)
//  private[this] val count     = ctx.newVar(0)
  private[this] val _hasNext  = ctx.newVar(false)
  private[this] val _valid    = ctx.newVar(false)

  final def hasNext(implicit tx: Tx): Boolean = {
    validate()
    _hasNext()
  }

  final def reset()(implicit tx: Tx): Unit =
    _valid() = false

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

  final def next()(implicit tx: Tx): T#Out[Tx] = {
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
/*
 *  IndexOfSlice.scala
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

import de.sciss.patterns.Types.{IntTop, Top}

import scala.collection.mutable

final case class IndexOfSlice[T1 <: Top, T2 <: Top](in: Pat[T1], sub: Pat[T2], from: Pat.Int) extends Pattern[IntTop] {
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, Int] = new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, Int] {
    private[this] val inStream    = in  .expand(ctx, tx0)
    private[this] val subStream   = sub .expand(ctx, tx0)
    private[this] val fromStream  = from.expand(ctx, tx0)
    private[this] val fromValue   = ctx.newVar(0)

    private[this] val _valid      = ctx.newVar(false)
    private[this] val _hasNext    = ctx.newVar(false)

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid() = true
        val hn = fromStream.hasNext
        _hasNext() = hn
        if (hn) {
          val _fromValue = math.max(0, fromStream.next())
          fromValue() = _fromValue
        }
      }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): Int = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      _hasNext()      = false   // there is only one run through

      val subVec      = subStream.toVector
      val subSz       = subVec.size
      val _fromValue  = fromValue()
      var idx = 0
      while (idx < _fromValue && inStream.hasNext) {
        inStream.next()
        idx += 1
      }
      if (idx < _fromValue) return -1

      val q   = mutable.Queue.empty[T1#Out[Tx]]
      var qSz = 0
      while (qSz < subSz && inStream.hasNext) {
        val c = inStream.next()
        q.enqueue(c)
        qSz += 1
      }

      if (qSz < subSz) return -1
      assert(q.size == subSz)

      while (q != subVec) {
        if (!inStream.hasNext) return -1
        val c = inStream.next()
        q.dequeue()
        q.enqueue(c)
        idx += 1
      }

      idx
    }
  }
}
/*
 *  Par.scala
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

import de.sciss.patterns.graph.impl.TimeRef

import scala.collection.immutable.{SortedMap => ISortedMap}

/** A simplified version of `Ppar`.*/
final case class Par(in: Pat[Pat[Event]])
  extends Pattern[Event] {

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, Event] = new StreamImpl(tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[Event] = {
    val inT = t(in)
    if (inT.eq(in)) this else copy(in = inT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, Event] {
    private[this] val inStream    = in.expand(ctx, tx0)

    private[this] val pq          = ctx.newVar[ISortedMap[TimeRef, Stream[Tx, Event]]](null)
    private[this] val _hasNext    = ctx.newVar[Boolean ](false)
    private[this] val elem        = ctx.newVar[Event   ](null)

    private[this] val _valid      = ctx.newVar(false)

    def reset()(implicit tx: Tx): Unit = if (_valid()) {
      _valid() = false
      inStream.reset()
    }

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()  = true
        pq()      = ISortedMap.empty

        var refCnt = 0
        inStream.foreach { pat =>
          val it = pat.expand[Tx]
          if (it.hasNext) {
            pq() = pq() + (new TimeRef(refCnt) -> it)
            refCnt += 1
          }
        }

        advance()
      }

    private def advance()(implicit tx: Tx): Unit =
      if (pq().nonEmpty) {
        val (ref, it) = pq().head
        pq()      = pq().tail
        val _elem = it.next()

        elem()    = _elem
        val d     = math.max(0.0, Event.delta(_elem))
        val now   = ref.time
        ref.time += d
        if (it.hasNext) pq() = pq() + (ref -> it)
        if (pq().nonEmpty) {
          val nextTime = pq().firstKey.time
          elem() = elem() + (Event.keyDelta -> (nextTime - now))
        }
        _hasNext() = true

      } else {
        _hasNext() = false
      }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): Event = {
      if (!hasNext) Stream.exhausted()
      val res = elem()
      advance()
      res
    }
  }
}
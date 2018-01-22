/*
 *  Ppar.scala
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

final case class Ppar(list: Pat[Pat.Event], repeats: Pat.Int = 1, offset: Pat.Int = 0)
  extends Pattern[Event] {

  type EOut = Event#COut

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, EOut] = new StreamImpl(tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, EOut] {
    private[this] val listStream: Stream[Tx, Stream[Tx, Map[String, _]]] = list.expand(ctx, tx0)
    private[this] val repeatsStream = repeats .expand(ctx, tx0)
    private[this] val offsetStream  = offset  .expand(ctx, tx0)

    private[this] val pq          = ctx.newVar[ISortedMap[TimeRef, Stream[Tx, EOut]]](null)
    private[this] val _hasNext    = ctx.newVar[Boolean](false)
    private[this] val repeatsVal  = ctx.newVar[Int    ](0)
    private[this] val offsetVal   = ctx.newVar[Int    ](0)
    private[this] val elem        = ctx.newVar[EOut   ](null)

    private[this] val _valid      = ctx.newVar(false)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid() = true
        pq() = ISortedMap.empty
        _hasNext() = repeatsStream.hasNext && offsetStream.hasNext
        if (!_hasNext()) return

        repeatsVal() = repeatsStream.next()
        offsetVal () = offsetStream .next()
        if (repeatsVal() != 1) throw new NotImplementedError("Ppar repeats")
        if (offsetVal () != 0) throw new NotImplementedError("Ppar offset")

        var refCnt = 0
        listStream.foreach { it =>
          if (it.hasNext) {
            pq() = pq() + (new TimeRef(refCnt) -> it)
            refCnt += 1
          }
        }

        advance()
      }

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

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

      } else {
        _hasNext() = false
      }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): EOut = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = elem()
      advance()
      res
    }
  }
}
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

  type EOut = Event#Out

  def iterator(implicit ctx: Context): Stream[EOut] = new Stream[EOut] {
    private[this] val listStream    = list    .expand
    private[this] val repeatsStream = repeats .expand
    private[this] val offsetStream  = offset  .expand
    private[this] var pq: ISortedMap[TimeRef, Stream[EOut]] = _

    private[this] var _hasNext  : Boolean = _
    private[this] var repeatsVal: Int     = _
    private[this] var offsetVal : Int     = _
    private[this] var elem      : EOut    = _

    def reset(): Unit = {
      pq = ISortedMap.empty
      _hasNext = repeatsStream.hasNext && offsetStream.hasNext
      if (!_hasNext) return

      repeatsVal = repeatsStream.next()
      offsetVal  = offsetStream .next()
      if (repeatsVal != 1) throw new NotImplementedError("Ppar repeats")
      if (offsetVal  != 0) throw new NotImplementedError("Ppar offset")

      var refCnt = 0
      listStream.foreach { it =>
        if (it.hasNext) {
          pq += new TimeRef(refCnt) -> it
          refCnt += 1
        }
      }

      advance()
    }

    reset()

    private def advance(): Unit =
      if (pq.nonEmpty) {
        val (ref, it) = pq.head
        pq        = pq.tail
        val _elem = it.next()

        elem      = _elem
        val d     = math.max(0.0, Event.delta(_elem))
        val now   = ref.time
        ref.time += d
        if (it.hasNext) pq += ref -> it
        if (pq.nonEmpty) {
          val nextTime = pq.firstKey.time
          elem += Event.keyDelta -> (nextTime - now)
        }

      } else {
        _hasNext = false
      }

    def hasNext: Boolean = _hasNext

    def next(): EOut = {
      if (!_hasNext) Stream.exhausted()
      val res = elem
      advance()
      res
    }
  }
}
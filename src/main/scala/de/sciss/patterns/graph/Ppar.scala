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

final case class Ppar(list: Seq[Pat.Event], repeats: Pat.Int = 1, offset : Pat.Int = 0)
  extends Pattern[Event] {

  type EOut = Event#Out

  def iterator(implicit ctx: Context): Stream[EOut] = {
    var refCnt = 0
    var pq0 = ISortedMap.empty[TimeRef, Stream[EOut]]
    list.foreach { pat =>
      val it = pat.expand
      if (it.hasNext) {
        pq0 += new TimeRef(refCnt) -> it
        refCnt += 1
      }
    }

    if (repeats.expand.next() != 1) throw new NotImplementedError("Ppar repeats")
    if (offset .expand.next() != 0) throw new NotImplementedError("Ppar offset")

    new Stream[EOut] {
      private[this] var pq    = pq0
      private[this] var done  = pq0.isEmpty
      private[this] var elem: EOut = _

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
          done = true
        }

      def hasNext: Boolean = !done

      advance()

      def reset(): Unit = ???

      def next(): EOut = {
        if (done) Stream.exhausted()
        val res = elem
        advance()
        res
      }
    }
  }
}
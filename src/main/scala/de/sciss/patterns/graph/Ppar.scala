/*
 *  Ppar.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
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

import scala.collection.AbstractIterator
import scala.collection.immutable.{SortedMap => ISortedMap}

final case class Ppar(list: Seq[Pat.Event], repeats: Pat.Int = 1, offset : Pat.Int = 0)
  extends Pattern[Event] {

  type Out = Event#Out

  def iterator(implicit ctx: Context): Iterator[Out] = {
    var refCnt = 0
    var pq0 = ISortedMap.empty[TimeRef, Iterator[Out]]
    list.foreach { pat =>
      val it = pat.expand
      if (it.hasNext) {
        pq0 += new TimeRef(refCnt) -> it
        refCnt += 1
      }
    }

    new AbstractIterator[Out] {
      private[this] var pq    = pq0
      private[this] var done  = pq0.isEmpty
      private[this] var elem: Out = _

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

      def next(): Out = {
        if (done) throw new NoSuchElementException("next on empty iterator")
        val res = elem
        advance()
        res
      }
    }
  }
}
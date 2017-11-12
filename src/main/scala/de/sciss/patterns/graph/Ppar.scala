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

import scala.collection.{AbstractIterator, breakOut}
import scala.collection.immutable.{SortedMap => ISortedMap}

final case class Ppar(list: Seq[Pat.Event], repeats: Pat.Int = 1, offset : Pat.Int = 0)
  extends Pattern[Event] {

  type Out = Event#Out

  def iterator(implicit ctx: Context): Iterator[Out] = {
    val pq0: ISortedMap[Double, Iterator[Out]] = list.flatMap { pat =>
      val it = pat.expand
      if (it.hasNext) Some(0.0 -> it) else None
    } (breakOut)

    new AbstractIterator[Out] {
      private[this] var pq    = pq0
      private[this] var done  = pq0.isEmpty
      private[this] var elem: Out = _

      private def advance(): Unit =
        pq.headOption match {
          case Some((time, it)) =>
            elem  = it.next()
            val d = Event.delta(elem)
            if (d >= 0.0 && it.hasNext) pq = pq.tail + ((time + d) -> it)

          case None => done = true
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
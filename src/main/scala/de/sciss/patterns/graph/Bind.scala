package de.sciss.patterns
package graph

import scala.collection.{AbstractIterator, breakOut}

final case class Bind(entries: (String, Pat[_])*) extends Pattern[Event] {
  type Out = Event#Out

  def iterator(implicit ctx: Context): Iterator[Out] = {
    val mapE: Map[String, Iterator[_]] = entries.map { case (key, value) => key -> value.embed } (breakOut)

    def checkNext(): Boolean = mapE.forall(_._2.hasNext)

    new AbstractIterator[Out] {
      var hasNext: Boolean = checkNext()

      private def mkState(): Out = mapE.map { case (key, value) => key -> value.next() }

      def next(): Out = {
        if (!hasNext) throw new NoSuchElementException("next on empty iterator")
        val res = mkState()
        hasNext = checkNext()
        res
      }
    }
  }
}

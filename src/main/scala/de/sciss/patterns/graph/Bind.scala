/*
 *  Bind.scala
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

final case class Bind(entries: (String, Pat[_])*) extends Pattern[Event] {
  type Out = Event#Out

  def iterator(implicit ctx: Context): Stream[Out] = {
    val mapE: Map[String, Stream[_]] = entries.map { case (key, value) => key -> value.expand } .toMap  // (breakOut)

    def checkNext(): Boolean = mapE.forall(_._2.hasNext)

    new Stream[Out] {
      var hasNext: Boolean = checkNext()

      def reset(): Unit = ???

      private def mkState(): Out = mapE.map { case (key, value) => key -> value.next() }

      def next(): Out = {
        if (!hasNext) Stream.exhausted()
        val res = mkState()
        hasNext = checkNext()
        res
      }
    }
  }
}

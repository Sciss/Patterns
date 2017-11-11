/*
 *  SeriesLike.scala
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
package impl

import de.sciss.patterns.Types.{Bridge, Top}

import scala.collection.AbstractIterator

trait SeriesLike[T1 <: Top, T2 <: Top, T <: Top] extends Pattern[T] {
  // ---- abstract ----

  def start: Pat[T1]

  protected val br: Bridge[T1, T2, T]

  protected def step: Pat[T2]

  protected def op(a: T#Out, b: T#Out): T#Out

  // ---- impl ----

  final def iterator(implicit ctx: Context): Iterator[T#Out] = {
    val ai = start.expand.map(br.lift1)
    val bi = step .expand.map(br.lift2)

    if (ai.isEmpty) Iterator.empty
    else new AbstractIterator[T#Out] {
      private[this] var state: T#Out = ai.next
      var hasNext = true

      def next(): T#Out = {
        val res = state
        hasNext = ai.hasNext && bi.hasNext
        if (hasNext) {
          state = op(state, bi.next())
        }
        res
      }
    }
  }
}

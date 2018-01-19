/*
 *  Sorted.scala
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

import de.sciss.patterns.Types.{Aux, Ord, Top}

final case class Sorted[T <: Top](in: Pat[T])(implicit ord: Ord[T]) extends Pattern[T] {
  override private[patterns] def aux: List[Aux] = ord :: Nil

  def iterator(implicit ctx: Context): Stream[T#Out] = new Stream[T#Out] {
    private[this] val inStream = in.expand

    private[this] var sortedIt: Iterator[T#Out] = _

    def reset(): Unit = {
      val xs    = inStream.toList
      sortedIt  = xs.sorted.iterator
    }

    reset()

    def hasNext: Boolean = sortedIt.hasNext

    def next(): T#Out = sortedIt.next()
  }
}

/*
 *  Indices.scala
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

import de.sciss.patterns.Types.{IntTop, Top}

case class Indices[T <: Top](in: Pat[T]) extends Pattern[IntTop] {
  def iterator(implicit ctx: Context): Stream[Int] = new Stream[Int] {
    private[this] val inStream = in.expand

    private[this] var count: Int = _

    def reset(): Unit =
      count = 0

    reset()

    def hasNext: Boolean = inStream.hasNext

    def next(): Int = {
      val res = count
      inStream.next()
      count += 1
      res
    }
  }
}

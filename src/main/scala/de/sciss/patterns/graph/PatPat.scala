/*
 *  PatPat.scala
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

import de.sciss.patterns.Types.Top

final case class PatPat[T <: Top](in: Seq[Pat[T]]) extends Pattern[Pat[T]] {
  def iterator(implicit ctx: Context): Stream[Stream[T#Out]] = new Stream[Stream[T#Out]] {
    private[this] val inStreams: Array[Stream[T#Out]] = in.iterator.map(_.expand).toArray
    private[this] var inIdx: Int = _

    def reset(): Unit = {
      inIdx = 0
    }

    def hasNext: Boolean = inIdx < inStreams.length // && inStreams(inIdx).hasNext

    def next(): Stream[T#Out] = {
      if (!hasNext) Stream.exhausted()
      val res = inStreams(inIdx)
//      val res   = s.next()
//      while (!s.hasNext && inIdx < inStreams.length) {
//        inIdx += 1
//        s = inStreams(inIdx)
//      }
      res
    }
  }
}

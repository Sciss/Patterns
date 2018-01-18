/*
 *  PatMap.scala
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

import scala.annotation.tailrec

final case class PatMap[T1 <: Top, T <: Top](outer: Pat[Pat[T1]], it: It[T1], inner: Graph[T])
  extends Pattern[Pat[T]] {

  def iterator(implicit ctx: Context): Stream[Stream[T#Out]] = new Stream[Stream[T#Out]] {
    private[this] val outerStream: Stream[Stream[T1#Out]] = outer.expand
    private[this] lazy val innerStream: Stream[T#Out]     = inner.expand
    private[this] var mapStream  : Stream[T#Out]          = _
    private[this] var hasMapStream = false

    private[this] var _hasNext: Boolean = _

    def reset(): Unit = {
      _hasNext = outerStream.hasNext
//      advance()
    }

    reset()

    def hasNext: Boolean = _hasNext

    private final class MapStream(itStream: Stream[T1#Out]) extends Stream[T#Out] {
      ctx.setOuterStream[T1#Out](it.token, itStream)

      def reset(): Unit = ()

      def hasNext: Boolean = itStream.hasNext && innerStream.hasNext

      def next(): T#Out = innerStream.next()
    }

    private def advance(): Unit = {
      if (!hasMapStream) {
        _hasNext = outerStream.hasNext
        if (_hasNext) {
          val itStream  = outerStream.next()
          mapStream     = new MapStream(itStream)
          hasMapStream  = true
          innerStream.reset()
        }
      }
    }

    def next(): Stream[T#Out] = {
      advance()
      if (!_hasNext) Stream.exhausted()
      val res = mapStream
      hasMapStream = false
      _hasNext = outerStream.hasNext
//      advance()
      res
    }
  }
}

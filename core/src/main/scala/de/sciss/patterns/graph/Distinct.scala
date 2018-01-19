/*
 *  Distinct.scala
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

final case class Distinct[T <: Top](in: Pat[T]) extends Pattern[T] {
  def iterator(implicit ctx: Context): Stream[T#Out] = new Stream[T#Out] {
    private[this] val inStream = in.expand

    private[this] var seen    : Set[T#Out]  = _
    private[this] var _hasNext: Boolean     = _
    private[this] var _next   : T#Out       = _

    def reset(): Unit = {
      seen = Set.empty
      advance()
    }

    @tailrec
    private def advance(): Unit = {
      _hasNext = inStream.hasNext
      if (_hasNext) {
        _next     = inStream.next()
        _hasNext  = !seen.contains(_next)
        if (_hasNext) {
          seen += _next
        } else {
          advance()
        }
      }
    }

    reset()

    def hasNext: Boolean = _hasNext

    def next(): T#Out = {
      if (!_hasNext) Stream.exhausted()
      val res = _next
      advance()
      res
    }
  }
}

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

import scala.annotation.tailrec

final case class Flatten[T <: Top](in: Pat[Pat[T]]) extends Pattern[T] {
  def iterator(implicit ctx: Context): Stream[T#Out] = new Stream[T#Out] {
    private[this] val inStream = in.expand
    private[this] var hasInner = false

    private[this] var innerStream : Stream[T#Out] = _
    private[this] var _hasNext    : Boolean       = _

    def reset(): Unit = advance()

    @tailrec
    private def advance(): Unit = {
      if (hasInner) {
        hasInner = innerStream.hasNext
      }

      _hasNext = hasInner
      if (!_hasNext) {
        _hasNext = inStream.hasNext
        if (_hasNext) {
          innerStream = inStream.next()
          hasInner = true
          advance()
        }
      }
    }

    reset()

    def hasNext: Boolean = _hasNext

    def next(): T#Out = {
      if (!_hasNext) Stream.exhausted()
      val res = innerStream.next()
      advance()
      res
    }
  }
}

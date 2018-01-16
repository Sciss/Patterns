/*
 *  SeqFill.scala
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

/*

    Pat.seqFill(4) { _ =>
      val b = Brown(0, 100, 2)
      b.take(10)
    }

 */
final case class SeqFill[T <: Top](n: Pat.Int, inner: Graph[T]) extends Pattern[T] {
  def iterator(implicit ctx: Context): Stream[T#Out] = new Stream[T#Out] {
    private[this] val nStream     = n    .expand
    private[this] val innerStream = inner.expand

    private[this] var _hasNext: Boolean = _

    def hasNext: Boolean = _hasNext

    private[this] var iteration = 0
    private[this] var nValue: Int = _

    def reset(): Unit = {
      _hasNext = nStream.hasNext
      if (_hasNext) {
        nValue    = nStream.next()
        iteration = 0
        nextIteration()
      }
    }

    @tailrec
    private def nextIteration(): Unit = {
      _hasNext = iteration < nValue
      if (_hasNext) {
        innerStream.reset()
        _hasNext = innerStream.hasNext
        if (!_hasNext) {
          iteration += 1
          nextIteration()
        }
      }
    }

    reset()

    def next(): T#Out = {
      if (!_hasNext) Stream.exhausted()
      val res = innerStream.next()
      _hasNext = innerStream.hasNext
      if (!_hasNext && iteration < nValue) {
        iteration += 1
        nextIteration()
      }
      res
    }
  }
}

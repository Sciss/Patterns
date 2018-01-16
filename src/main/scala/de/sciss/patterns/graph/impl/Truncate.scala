/*
 *  Truncate.scala
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
package impl

import de.sciss.patterns.Types.{IntTop, Top}

trait Truncate[T <: Top] extends Pattern[T] {
  // ---- abstract ----

  protected val in: Pat[T]
  protected def length: Pat[IntTop]

  protected def truncate(inStream: Stream[T#Out], n: Int): Stream[T#Out]

  // ---- impl ----

  def iterator(implicit ctx: Context): Stream[T#Out] = new Stream[T#Out] {
    private[this] val lenStream = length.expand
    private[this] val inStream  = in    .expand

    private[this] var peer    : Stream[T#Out] = _
    private[this] var _hasNext: Boolean       = _

    def reset(): Unit = {
      _hasNext = lenStream.hasNext
      if (_hasNext) {
        val lenVal = lenStream.next()
        peer = truncate(inStream, lenVal)
        _hasNext = peer.hasNext
        // if (!_hasNext) reset()
      }
    }

    reset()

    def hasNext: Boolean = _hasNext

    def next(): T#Out = {
      if (!_hasNext) Stream.exhausted()
      val res = peer.next()
      _hasNext = peer.hasNext
      res
    }
  }
}

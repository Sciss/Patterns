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

trait TruncateNew[T <: Top] extends Pattern[T] {
  // ---- abstract ----

  protected val in: Pat[T]
  protected def length: Pat[IntTop]

  protected def truncate(inStream: StreamNew[T#Out], n: Int): StreamNew[T#Out]

  // ---- impl ----

  def iterator(implicit ctx: Context): Stream[T#Out] = ???

  def iteratorNew(implicit ctx: Context): StreamNew[T#Out] = new StreamNew[T#Out] {
    private[this] val lenStream = ??? : StreamNew[Int] // length.expand
    private[this] val inStream  = ??? : StreamNew[T#Out] // in    .expand

    private[this] var peer    : StreamNew[T#Out] = _
    private[this] var _hasNext: Boolean       = _

    private[this] var RESET     = true

    def reset(): Unit =
      RESET = true

    def moveNext(): Boolean = {
      if (RESET) {
        _hasNext = lenStream.moveNext()
        if (_hasNext) {
          val lenVal = lenStream.current
          peer = truncate(inStream, lenVal)
        }
      }
      _hasNext &&= peer.moveNext()
      _hasNext
    }

    def current: T#Out = {
      if (RESET || !_hasNext) Stream.exhausted()
      peer.current
    }
  }
}

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

  protected def truncate[Tx](inStream: Stream[Tx, T#TOut[Tx]], n: Int)(implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#TOut[Tx]]

  // ---- impl ----

  def iterator[Tx](implicit ctx: Context[Tx]): Stream[Tx, T#TOut[Tx]] = new Stream[Tx, T#TOut[Tx]] {
    private[this] val lenStream = length.expand
    private[this] val inStream  = in    .expand

    private[this] val peer      = ctx.newVar[Stream[Tx, T#TOut[Tx]]](null)
    private[this] val _hasNext  = ctx.newVar(false)

    private[this] val _valid    = ctx.newVar(false)

    def reset()(implicit tx: Tx): Unit = _valid() = false

    private def validate()(implicit tx: Tx): Unit = if (!_valid()) {
      _valid() = true
      _hasNext() = lenStream.hasNext
      if (_hasNext()) {
        val lenVal = lenStream.next()
        peer() = truncate(inStream, lenVal)
        _hasNext() = peer().hasNext
      }
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): T#TOut[Tx] = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = peer().next()
      _hasNext() = peer().hasNext
      res
    }
  }
}

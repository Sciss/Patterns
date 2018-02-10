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

trait Truncate[A] extends Pattern[A] {
  // ---- abstract ----

  protected val in    : Pat[A]
  protected def length: Pat[Int]

  protected def truncate[Tx](inStream: Stream[Tx, A], n: Int)(implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A]

  // ---- impl ----

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl(tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val lenStream = length.expand(ctx, tx0)
    private[this] val inStream  = in    .expand(ctx, tx0)

    private[this] val peer      = ctx.newVar[Stream[Tx, A]](null)
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

    def next()(implicit tx: Tx): A = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = peer().next()
      _hasNext() = peer().hasNext
      res
    }
  }
}

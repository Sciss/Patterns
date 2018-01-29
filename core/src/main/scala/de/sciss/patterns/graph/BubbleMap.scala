/*
 *  BubbleMap.scala
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

final case class BubbleMap[T1 <: Top, T <: Top](outer: Pat[T1], it: It[T1], inner: Graph[T])
  extends Pattern[T] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = {
    logStream("BubbleMap.iterator")
    new StreamImpl(tx)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    private[this] val outerStream: Stream[Tx, T1#Out[Tx]]  = outer.expand(ctx, tx0)
    private[this] val innerStream: Stream[Tx, T #Out[Tx]]  = inner.expand(ctx, tx0)

    private[this] val _valid        = ctx.newVar(false)
    private[this] val _hasNext      = ctx.newVar(false)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid() = true
        logStream("BubbleMap.iterator.validate()")
        advance()
      }

    def reset()(implicit tx: Tx): Unit = {
      _valid() = false
      // innerStream.reset()
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      val res = _hasNext()
      logStream(s"BubbleMap.iterator.hasNext = $res")
      res
    }

    private def advance()(implicit tx: Tx): Unit = {
      val ohn = outerStream.hasNext
      _hasNext() = ohn
      logStream(s"BubbleMap.iterator.advance(); outerStream.hasNext = $ohn")
      if (ohn) {
        val itValue     = outerStream.next()
        logStream(s"BubbleMap.iterator.advance(); itValue = $itValue; token = ${it.token}")
        val itStream    = Stream.single(itValue)
        ctx.setOuterStream[T1#Out[Tx]](it.token, itStream)
        innerStream.reset()
        val ihn = innerStream.hasNext
        _hasNext() = ihn
        logStream(s"BubbleMap.iterator.advance(); innerStream.hasNext = $ihn")
      }
    }

    def next()(implicit tx: Tx): T#Out[Tx] = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res     = innerStream.next()
      val ihn     = innerStream.hasNext
      logStream(s"BubbleMap.iterator.next(); hasNext = $ihn; res = $res")
      _hasNext()  = ihn
      if (!ihn) reset() // advance()
      res
    }
  }
}

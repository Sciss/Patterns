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

import de.sciss.patterns.Types.{IntTop, Top}

import scala.annotation.tailrec

/*

    Pat.seqFill(4) { _ =>
      val b = Brown(0, 100, 2)
      b.take(10)
    }

 */
final case class SeqFill[T <: Top](n: Pat.Int, inner: Graph[T], it: It[IntTop]) extends Pattern[T] {
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = new StreamImpl(tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    @transient final private[this] lazy val ref = new AnyRef

    private[this] val mkItStream = { () =>
      val res = new ItStreamImpl
      ctx.addStream(ref, res)
      res
    }

    ctx.provideOuterStream(it.token, mkItStream)(tx0)

    private[this] val nStream     = n    .expand(ctx, tx0)
    private[this] val innerStream = inner.expand(ctx, tx0)

    private[this] val iteration   = ctx.newVar(0)
    private[this] val nValue      = ctx.newVar(0)

    private[this] val _hasNext    = ctx.newVar(false)
    private[this] val _valid      = ctx.newVar(false)

    private final class ItStreamImpl extends Stream[Tx, Int] {
      private[this] val _hasNext = ctx.newVar(true)

      def reset()(implicit tx: Tx): Unit    = _hasNext() = true
      def hasNext(implicit tx: Tx): Boolean = _hasNext()

      def next()(implicit tx: Tx): Int = {
        if (!hasNext) Stream.exhausted()
        _hasNext() = false
        iteration()
      }
    }

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()      = true
        _hasNext()    = nStream.hasNext
        if (_hasNext()) {
          nValue()    = nStream.next()
          iteration() = 0
          nextIteration()
        }
      }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

    @tailrec
    private def nextIteration()(implicit tx: Tx): Unit = {
      val i = iteration()
      _hasNext() = i < nValue()
      if (_hasNext()) {
//        val itStream = () => Stream.single(i)
//        ctx.provideOuterStream(it.token, itStream)
        ctx.getStreams(ref).foreach(_.reset())
        innerStream.reset()
        _hasNext() = innerStream.hasNext
        if (!_hasNext()) {
          iteration() = i + 1
          nextIteration()
        }
      }
    }

    def next()(implicit tx: Tx): T#Out[Tx] = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = innerStream.next()
      _hasNext() = innerStream.hasNext
      if (!_hasNext() && iteration() < nValue()) {
        iteration() = iteration() + 1
        nextIteration()
      }
      res
    }
  }
}

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
import de.sciss.patterns.graph.impl.MapIterationStream

final case class PatMap[T1 <: Top, T <: Top](outer: Pat[Pat[T1]], it: It[T1], inner: Graph[T])
  extends Pattern[Pat[T]] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, Stream[Tx, T#Out[Tx]]] = {
    logStream("PatMap.iterator")
    new StreamImpl(tx)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, Stream[Tx, T#Out[Tx]]] {
    @transient final private[this] lazy val ref = new AnyRef

    private def mkItStream(implicit tx: Tx) = {
      val res = new MapIterationStream(outer, tx)
      ctx.addStream(ref, res)
      res
    }

    ctx.provideOuterStream(it.token, mkItStream(_))(tx0)

    private[this] val innerStream: Stream[Tx, T#Out[Tx]] = inner.expand(ctx, tx0)

    private[this] val mapStream     = ctx.newVar[Stream[Tx, T#Out[Tx]]](null)
    private[this] val _valid        = ctx.newVar(false)
    private[this] val _hasNext      = ctx.newVar(false)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid() = true
        logStream("PatMap.iterator.validate()")
        advance()
      }

    def reset()(implicit tx: Tx): Unit = {
      _valid() = false
      logStream("PatMap.iterator.reset()")
    }

    private def advance()(implicit tx: Tx): Unit = {
      ctx.getStreams(ref).foreach(_.reset())
      innerStream.reset()
      _hasNext() = innerStream.hasNext
      if (_hasNext()) {
        val b = Vector.newBuilder[T#Out[Tx]]
        var i = 0
        // there is _no_ reasonable way to provide the
        // stream than to eagerly collect the values here,
        // because of the order of execution between inner and outer
        // `next`!
        while (innerStream.hasNext) {
          b += innerStream.next()
          i += 1
        }
        val inner   = Stream[Tx, T#Out[Tx]](b.result: _*)
        mapStream() = inner
        _hasNext()  = inner.hasNext
      }
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): Stream[Tx, T#Out[Tx]] = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = mapStream()
      advance()
      res
    }
  }
}

/*
 *  FlatMap.scala
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

final case class FlatMap[T1 <: Top, T <: Top](outer: Pat[Pat[T1]], it: It[T1], inner: Graph[T])
  extends Pattern[T] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = {
    logStream("FlatMap.iterator")
    new StreamImpl(tx)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    @transient final private[this] lazy val ref = new AnyRef

    private def mkItStream(implicit tx: Tx) = {
      val res = new MapIterationStream(outer, tx)
      ctx.addStream(ref, res)
      res
    }

    ctx.provideOuterStream(it.token, mkItStream(_))(tx0)

    private[this] val innerStream: Stream[Tx, T#Out[Tx]] = inner.expand(ctx, tx0)

    private[this] val _valid        = ctx.newVar(false)
    private[this] val _hasNext      = ctx.newVar(false)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid() = true
        logStream("FlatMap.iterator.validate()")
        advance()
      }

    def reset()(implicit tx: Tx): Unit = {
      _valid() = false
      logStream("FlatMap.iterator.reset()")
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    private def advance()(implicit tx: Tx): Unit = {
      ctx.getStreams(ref).foreach(_.reset())
      innerStream.reset()
      _hasNext() = innerStream.hasNext
    }

    def next()(implicit tx: Tx): T#Out[Tx] = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = innerStream.next()
      logStream(s"FlatMap.iterator.next() = $res")
      if (!innerStream.hasNext) advance()
      res
    }
  }
}

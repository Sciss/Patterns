/*
 *  Filter.scala
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

import de.sciss.patterns.graph.impl.MapItStream

final case class Filter[A](outer: Pat[Pat[A]], it: It[A], inner: Pat[Boolean])
  extends Pattern[Pat[A]] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, Pat[A]] = {
    logStream("Filter.iterator")
    new StreamImpl(tx)
  }

  def transform(t: Transform): Pat[Pat[A]] = ???

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, Pat[A]] {
    @transient final private[this] lazy val ref = new AnyRef

    private def mkItStream(implicit tx: Tx): Stream[Tx, A] = {
      val res = new MapItStream(outer, tx)
      ctx.addStream(ref, res)
      res
    }

    ctx.provideOuterStream(it.token, mkItStream(_))(tx0)

    private[this] val innerStream: Stream[Tx, Boolean] = inner.expand(ctx, tx0)

    // because `inner` is not guaranteed to depend on `It`, we must
    // pro-active create one instance of the it-stream which is used
    // as an additional constraint to determine `hasNext`!
    private[this] val itStream      = mkItStream(tx0)

    private[this] val mapStream     = ctx.newVar[Pat[A]](null) // Stream[Tx, T#Out[Tx]]](null)
    private[this] val _valid        = ctx.newVar(false)
    private[this] val _hasNext      = ctx.newVar(false)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        logStream("Filter.iterator.validate()")
        _valid() = true
        buildNext() // advance()
      }

    def reset()(implicit tx: Tx): Unit = {
      logStream("Filter.iterator.reset()")
      _valid() = false
      ctx.getStreams(ref).foreach {
        case m: MapItStream[Tx, _] => m.resetOuter()
        // case _ =>
      }
    }

    private def advance()(implicit tx: Tx): Unit = {
      ctx.getStreams(ref).foreach {
        case m: MapItStream[Tx, _] => m.advance()
        // case _ =>
      }
      innerStream.reset()
      buildNext()
    }

    private def buildNext()(implicit tx: Tx): Unit = {
      val hn = itStream.hasNext && innerStream.hasNext
      _hasNext() = hn
      if (hn) {
        // itStream.next()
        val b = Vector.newBuilder[A]
        var i = 0
        // there is _no_ reasonable way to provide the
        // stream than to eagerly collect the values here,
        // because of the order of execution between inner and outer
        // `next`!
        while (innerStream.hasNext) {
          ??? // b += innerStream.next()
          i += 1
        }
        val inner   = Stream[Tx, A](b.result: _*)
        mapStream() = ??? // inner
        _hasNext()  = inner.hasNext
      }
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): Pat[A] = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = mapStream()
      advance()
      res
    }
  }
}

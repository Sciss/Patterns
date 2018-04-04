/*
 *  MapWithIndex.scala
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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.impl.{IndexItStream, MapItStream}

final case class MapWithIndex[A1, A] private[patterns](outer: Pat[Pat[A1]], itIn: It[A1], itIdx: It[Int], inner: Pat[A])
  extends Pattern[Pat[A]] {

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] = {
    logStream("MapWithIndex.iterator")
    new StreamImpl(tx)
  }

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Pat[A]] = {
    val outerT  = t(outer)
    val innerT  = t(inner)
    if (outerT.eq(outer) && innerT.eq(inner)) this else {
      val (itT, innerT1) = itIn.replaceIn(innerT)
      copy(outer = outerT, itIn = itT, inner = innerT1)
    }
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, Pat[A]] {
    @transient final private[this] lazy val refIn   = new AnyRef
    @transient final private[this] lazy val refIdx  = new AnyRef

    private[this] val id          = tx0.newId()
    private[this] val iteration   = tx0.newIntVar(id, 0)
    private[this] val mapStream   = ??? : S#Var[Pat[A]] // ctx.newVar[Pat[A]](null)(tx0)
    private[this] val _valid      = tx0.newBooleanVar(id, false)
    private[this] val _hasNext    = tx0.newBooleanVar(id, false)

    private def mkItInStream(implicit tx: S#Tx): Stream[S, A1] = {
      val res = new MapItStream(outer, tx)
      ctx.addStream(refIn, res)
      res
    }

    private def mkItIdxStream(implicit tx: S#Tx): Stream[S, Int] = {
      val res = new IndexItStream(iteration, tx)
      ctx.addStream(refIdx, res)
      res
    }

    ctx.provideOuterStream[A1 ](itIn .token, mkItInStream (_))(tx0)
    ctx.provideOuterStream[Int](itIdx.token, mkItIdxStream(_))(tx0)

    private[this] val innerStream: Stream[S, A] = inner.expand(ctx, tx0)

    // because `inner` is not guaranteed to depend on `It`, we must
    // pro-active create one instance of the it-stream which is used
    // as an additional constraint to determine `hasNext`!
    private[this] val itInStream    = mkItInStream(tx0)

    private def validate()(implicit tx: S#Tx): Unit =
      if (!_valid()) {
        logStream("MapWithIndex.iterator.validate()")
        _valid()    = true
        iteration() = 0
        buildNext()
      }

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      //      logStream("MapWithIndex.iterator.reset()")
      _valid() = false
      ctx.getStreams(refIn).foreach {
        case m: MapItStream[S, _] => m.resetOuter()
        // case _ =>
      }
      innerStream.reset()
    }

    private def advance()(implicit tx: S#Tx): Unit = {
      ctx.getStreams(refIn).foreach {
        case m: MapItStream[S, _] => m.advance()
        // case _ =>
      }
      iteration() = iteration() + 1
      innerStream.reset()
      buildNext()
    }

    private def buildNext()(implicit tx: S#Tx): Unit = {
      val hn = itInStream.hasNext // && innerStream.hasNext
      _hasNext() = hn
      if (hn) {
        val itIdxStreams = ctx.getStreams(ref)
        itIdxStreams.foreach(_.reset())
        // itStream.next()
        val b = Vector.newBuilder[A]
        var i = 0
        // there is _no_ reasonable way to provide the
        // stream than to eagerly collect the values here,
        // because of the order of execution between inner and outer
        // `next`!
        while (innerStream.hasNext) {
          b += innerStream.next()
          i += 1
        }
        val inner   = Pat(b.result: _*) // Stream[S, A](b.result: _*)
        mapStream() = inner
        _hasNext()  = true // inner.hasNext
      }
    }

    def hasNext(implicit tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: S#Tx): Pat[A] = {
      if (!hasNext) Stream.exhausted()
      val res = mapStream()
      advance()
      res
    }
  }
}

///*
// *  Filter.scala
// *  (Patterns)
// *
// *  Copyright (c) 2017-2021 Hanns Holger Rutz. All rights reserved.
// *
// *	This software is published under the GNU Affero General Public License v3+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.patterns
//package graph
//
//import de.sciss.patterns.graph.impl.MapItStream
//
//final case class Filter[A](outer: Pat[Pat[A]], it: It[A], inner: Pat[Boolean])
//  extends Pattern[Pat[A]] {
//
//  def iterator[Tx](implicit ctx: Context[T], tx: T): Stream[T, Pat[A]] = {
//    logStream("Filter.iterator")
//    new StreamImpl(tx)
//  }
//
//  def transform(t: Transform): Pat[Pat[A]] = {
//    val outerT  = t(outer)
//    val innerT  = t(inner)
//    if (outerT.eq(outer) && innerT.eq(inner)) this else copy(outer = outerT, inner = innerT)
//  }
//
//  private final class StreamImpl[T <: Exec[T]](tx0: T)(implicit ctx: Context[T]) extends Stream[T, Pat[A]] {
//    @transient final private[this] lazy val ref = new AnyRef
//
//    private def mkItStream(implicit tx: T): Stream[T, A] = {
//      val res = new MapItStream(outer, tx)
//      ctx.addStream(ref, res)
//      res
//    }
//
//    ctx.provideOuterStream(it.token, mkItStream(_))(tx0)
//
//    private[this] val innerStream: Stream[T, Boolean] = inner.expand(ctx, tx0)
//
//    // because `inner` is not guaranteed to depend on `It`, we must
//    // pro-active create one instance of the it-stream which is used
//    // as an additional constraint to determine `hasNext`!
//    private[this] val itStream      = mkItStream(tx0)
//
//    private[this] val mapStream     = ctx.newVar[Pat[A]](null) // Stream[T, T#Out[Tx]]](null)
//    private[this] val _valid        = ctx.newVar(false)
//    private[this] val _hasNext      = ctx.newVar(false)
//
//    private def validate()(implicit tx: T): Unit =
//      if (!_valid()) {
//        logStream("Filter.iterator.validate()")
//        _valid() = true
//        buildNext() // advance()
//      }
//
//    def reset()(implicit tx: T): Unit = {
//      logStream("Filter.iterator.reset()")
//      _valid() = false
//      ctx.getStreams(ref).foreach {
//        case m: MapItStream[T, _] => m.resetOuter()
//        // case _ =>
//      }
//    }
//
//    private def advance()(implicit tx: T): Unit = {
//      ctx.getStreams(ref).foreach {
//        case m: MapItStream[T, _] => m.advance()
//        // case _ =>
//      }
//      innerStream.reset()
//      buildNext()
//    }
//
//    private def buildNext()(implicit tx: T): Unit = {
//      val hn = itStream.hasNext && innerStream.hasNext
//      _hasNext() = hn
//      if (hn) {
//        // itStream.next()
//        val b = Vector.newBuilder[A]
//        var i = 0
//        // there is _no_ reasonable way to provide the
//        // stream than to eagerly collect the values here,
//        // because of the order of execution between inner and outer
//        // `next`!
//        while (innerStream.hasNext) {
//          ... // b += innerStream.next()
//          i += 1
//        }
//        val inner   = Stream[T, A](b.result: _*)
//        mapStream() = ... // inner
//        _hasNext()  = inner.hasNext
//      }
//    }
//
//    def hasNext(implicit tx: T): Boolean = {
//      validate()
//      _hasNext()
//    }
//
//    def next()(implicit tx: T): Pat[A] = {
//      validate()
//      if (!_hasNext()) Stream.exhausted()
//      val res = mapStream()
//      advance()
//      res
//    }
//  }
//}

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

final case class FlatMap[T1 <: Top, T <: Top](outer: Pat[Pat[T1]], it: It[T1], inner: Graph[T])
  extends Pattern[T] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = {
    logStream("FlatMap.iterator")
    new StreamImpl(tx)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    private[this] val outerStream: Stream[Tx, Stream[Tx, T1#Out[Tx]]]  = outer.expand(ctx, tx0)
    private[this] val innerStream: Stream[Tx, T#Out[Tx]]               = inner.expand(ctx, tx0)

    private[this] val itMapInner    = ctx.newVar[Stream[Tx, T#Out[Tx]]](null)

    private[this] val hasMapStream  = ctx.newVar(false)
    private[this] val _valid        = ctx.newVar(false)
    private[this] val _hasNext      = ctx.newVar(false)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()    = true
        _hasNext()  = outerStream.hasNext
        logStream(s"FlatMap.iterator.validate(); hasNext = $hasNext")
        // advance()
      }

    def reset()(implicit tx: Tx): Unit = {
      _valid() = false
      logStream("FlatMap.iterator.reset()")
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    private final class InnerStream(itStream: Stream[Tx, T1#Out[Tx]]) extends Stream[Tx, T#Out[Tx]] {
      override def toString = s"FlatMap.iterator#InnerStream($itStream)"

      def init()(implicit tx: Tx): this.type = {
        logStream(s"FlatMap.iterator#InnerStream.init(); token = ${it.token}")
        ctx.setOuterStream[T1#Out[Tx]](it.token, itStream)
        this
      }

      def reset()(implicit tx: Tx): Unit = ()

      def hasNext(implicit tx: Tx): Boolean = itStream.hasNext && innerStream.hasNext

      def next()(implicit tx: Tx): T#Out[Tx] = {
        // XXX TODO --- how to get rid of the casting?
        val res = innerStream.next()(tx.asInstanceOf[Tx])
        logStream(s"FlatMap.iterator#InnerStream.next() = $res")
        res
      }
    }

    private def advance()(implicit tx: Tx): Unit = {
      val hm = hasMapStream()
      logStream(s"FlatMap.iterator.advance(); hasMapStream = $hm")
      if (!hm) {
        val hn = outerStream.hasNext
        _hasNext() = hn
        logStream(s"FlatMap.iterator.advance(); hasNext = $hn")
        if (hn) {
          val itStream    = outerStream.next()
          logStream(s"FlatMap.iterator.advance(); itStream = $itStream")
          itMapInner()    = new InnerStream(itStream).init()
          hasMapStream()  = true
          innerStream.reset()
        }
      }
    }

    def next()(implicit tx: Tx): T#Out[Tx] = {
      validate()
      advance()
      if (!_hasNext()) Stream.exhausted()
      val mi  = itMapInner()
      val res = mi.next()
      logStream(s"FlatMap.iterator.next() = $res")
      hasMapStream() = false
      _hasNext() = outerStream.hasNext
      res
    }
  }
}

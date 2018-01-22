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

final case class PatMap[T1 <: Top, T <: Top](outer: Pat[Pat[T1]], it: It[T1], inner: Graph[T])
  extends Pattern[Pat[T]] {

  def iterator[Tx](implicit ctx: Context[Tx]): Stream[Tx, Stream[Tx, T#TOut[Tx]]] = new Stream[Tx, Stream[Tx, T#TOut[Tx]]] {
    private[this] val outerStream: Stream[Tx, Stream[Tx, T1#TOut[Tx]]]  = outer.expand[Tx]
    private[this] val innerStream: Stream[Tx, T#TOut[Tx]]               = inner.expand[Tx]

    private[this] val itMapInner    = ctx.newVar[Stream[Tx, T#TOut[Tx]]](null)

    private[this] val hasMapStream  = ctx.newVar(false)
    private[this] val _valid        = ctx.newVar(false)
    private[this] val _hasNext      = ctx.newVar(false)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()    = true
        _hasNext()  = outerStream.hasNext
//        advance()
      }

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

//    private final case class PatMapInner(itPat: Pat[T1]) extends Pat[T] {
//
//      private[patterns] def aux: List[Aux] = Nil
//
//      private[patterns] def expand[Tx1](implicit ctx: Context[Tx1]): Stream[Tx1, T#TOut[Tx1]] = iterator
//      def embed                   [Tx1](implicit ctx: Context[Tx1]): Stream[Tx1, T#TOut[Tx1]] = iterator
//
//      def iterator[Tx1](implicit ctx: Context[Tx1]): Stream[Tx1, T#TOut[Tx1]] = new Stream[Tx1, T#TOut[Tx1]] {
//        private[this] val itStream: Stream[Tx1, T1#TOut[Tx1]] = itPat.expand
//
//        ctx.setOuterStream[T1#TOut[Tx1]](it.token, itStream)
//
//        def reset()(implicit tx: Tx1): Unit    = ()
//
//        // XXX TODO --- how to get rid of the casting?
//        def hasNext(implicit tx: Tx1): Boolean      = itStream.hasNext && innerStream.hasNext(tx.asInstanceOf[Tx])
//        def next ()(implicit tx: Tx1): T#TOut[Tx1]  = ??? // innerStream.next()(tx.asInstanceOf[Tx])
//      }
//    }

    private final class InnerStream(itStream: Stream[Tx, T1#TOut[Tx]]) extends Stream[Tx, T#TOut[Tx]] {
      ctx.setOuterStream[T1#TOut[Tx]](it.token, itStream)

      def reset()(implicit tx: Tx): Unit = ()

      // XXX TODO --- how to get rid of the casting?
      def hasNext(implicit tx: Tx): Boolean     = itStream.hasNext && innerStream.hasNext
      def next ()(implicit tx: Tx): T#TOut[Tx]  = innerStream.next()(tx.asInstanceOf[Tx])
    }

    private def advance()(implicit tx: Tx): Unit = {
      if (!hasMapStream()) {
        _hasNext() = outerStream.hasNext
        if (_hasNext()) {
          val itStream    = outerStream.next()
          itMapInner()   = new InnerStream(itStream)
          hasMapStream()  = true
          innerStream.reset()
        }
      }
    }

    def next()(implicit tx: Tx): Stream[Tx, T#TOut[Tx]] = {
      validate()
      advance()
      if (!_hasNext()) Stream.exhausted()
      val res = itMapInner()
      hasMapStream() = false
      _hasNext() = outerStream.hasNext
      res
    }
  }
}

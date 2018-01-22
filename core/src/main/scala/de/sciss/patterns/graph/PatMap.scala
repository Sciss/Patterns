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

  def iterator[Tx](implicit ctx: Context[Tx]): Stream[Tx, Pat[T] /* Stream[Tx, T#Out] */] = new Stream[Tx, Pat[T] /* Stream[Tx, T#Out] */] {
    private[this] val outerStream: Stream[Tx, Stream[Tx, T1#Out]] = ??? // outer.expand[Tx]
    private[this] val innerStream: Stream[Tx, T#Out]              = inner.expand[Tx]

    private[this] val mapStream  = ctx.newVar[Stream[Tx, T#Out]](null)

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

    private final class MapStream(itStream: Stream[Tx, T1#Out]) extends Stream[Tx, T#Out] {
      ctx.setOuterStream[T1#Out](it.token, itStream)

      def reset()(implicit tx: Tx): Unit = ()

      def hasNext(implicit tx: Tx): Boolean = itStream.hasNext && innerStream.hasNext

      def next()(implicit tx: Tx): T#Out = innerStream.next()
    }

    private def advance()(implicit tx: Tx): Unit = {
      if (!hasMapStream()) {
        _hasNext() = outerStream.hasNext
        if (_hasNext()) {
          val itStream    = outerStream.next()
          mapStream()     = new MapStream(itStream)
          hasMapStream()  = true
          innerStream.reset()
        }
      }
    }

    def next()(implicit tx: Tx): Pat[T] /* Stream[Tx, T#Out] */ = {
      validate()
      advance()
      if (!_hasNext()) Stream.exhausted()
      val res = mapStream()
      hasMapStream() = false
      _hasNext() = outerStream.hasNext
//      advance()
      res
      ???
    }
  }
}

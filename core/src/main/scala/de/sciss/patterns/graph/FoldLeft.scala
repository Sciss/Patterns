/*
 *  FoldLeft.scala
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

import de.sciss.patterns.Types.{Top, Tuple2Top}
import de.sciss.patterns.graph.impl.FoldLeftItStream

final case class FoldLeft[T1 <: Top, T <: Top](outer: Pat[Pat[T1]], z: Pat[T], it: It[Tuple2Top[T1, T]],
                                               inner: Graph[T])
  extends Pattern[T] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] =
    new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    @transient final private[this] lazy val ref = new AnyRef

    private def mkItStream(implicit tx: Tx) = {
      val res = new FoldLeftItStream[Tx, T1, T](outer, inner, z, tx)
      ctx.addStream(ref, res)
      res
    }

    ctx.provideOuterStream[(T1#Out[Tx], T#Out[Tx])](it.token, mkItStream(_))(tx0)

    // because `inner` is not guaranteed to depend on `It`, we must
    // pro-active create one instance of the it-stream which is used
    // as an additional constraint to determine `hasNext`!
    private[this] val itStream    = mkItStream(tx0)

    private[this] val _valid        = ctx.newVar(false)
    private[this] val _hasNext      = ctx.newVar(false)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        logStream("FoldLeft.iterator.validate()")
        _valid() = true
        val hn = itStream.hasNext
        _hasNext() = hn
        if (hn) {
          val itStreams = ctx.getStreams(ref)
          while (itStream.hasNext) {
//            itStream.next()
            itStreams.foreach {
              case m: FoldLeftItStream[Tx, T1, T] => m.advance()
            }
          }
//          _hasNext() = true
        }
      }

    def reset()(implicit tx: Tx): Unit = {
      logStream("FoldLeft.iterator.reset()")
      _valid() = false
      ctx.getStreams(ref).foreach {
        case m: FoldLeftItStream[Tx, T1, T] => m.resetOuter()
      }
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): T#Out[Tx] = {
      if (!hasNext) Stream.exhausted()
      val res = itStream.result
      logStream(s"FoldLeft.iterator.next() = $res")
      _hasNext() = false
      res
    }
  }
}
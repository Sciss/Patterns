/*
 *  SortWith.scala
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

import de.sciss.patterns.Types.{BooleanTop, Top, Tuple2Top}
import de.sciss.patterns.graph.impl.SortWithItStream

import scala.util.control.Breaks

final case class SortWith[T <: Top](outer: Pat[Pat[T]], it: It[Tuple2Top[T, T]], lt: Graph[BooleanTop])
  extends Pattern[Pat[T]] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, Stream[Tx, T#Out[Tx]]] =
    new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends Stream[Tx, Stream[Tx, T#Out[Tx]]] {

    type A = T#Out[Tx]

    @transient final private[this] lazy val ref = new AnyRef

    private def mkItStream(implicit tx: Tx) = {
      val res = new SortWithItStream[Tx, T](tx)
      ctx.addStream(ref, res)
      res
    }

    ctx.provideOuterStream[(A, A)](it.token, mkItStream(_))(tx0)

    private[this] val outerStream: Stream[Tx, Stream[Tx, A]]  = outer.expand(ctx, tx0)
    private[this] val ltStream   : Stream[Tx, Boolean]        = lt   .expand(ctx, tx0)

    private[this] val _valid      = ctx.newVar(false)

    private[this] val sortedIt    = ctx.newVar[Stream[Tx, Stream[Tx, A]]](null)
    private[this] val _hasSorted  = ctx.newVar(false)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()      = true
        _hasSorted()  = false
        perform()
      }

    private def perform()(implicit tx: Tx): Unit = {
      val vec: Vector[Vector[A]] = outerStream.map(_.toVector).toVector
      val itStreams = ctx.getStreams(ref)
      Breaks.breakable {
        val sorted = vec.sortWith { (x, y) =>
          itStreams.foreach {
            case m: SortWithItStream[Tx, T] => m.advance(x, y)
          }
          ltStream.reset()
          if (ltStream.hasNext) {
            ltStream.next()
          } else {
            Breaks.break()
          }
        }
        _hasSorted() = true
        sortedIt() = Stream(sorted.map(xs => Stream(xs: _*)): _*)
      }
    }

    def reset()(implicit tx: Tx): Unit = {
      _valid() = false
      println("SortWith. TODO: resetOuter")
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasSorted() && sortedIt().hasNext
    }

    def next()(implicit tx: Tx): Stream[Tx, T#Out[Tx]] = {
      if (!hasNext) Stream.exhausted()
      sortedIt().next()
    }
  }
}
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

import de.sciss.patterns.graph.impl.SortWithItStream

import scala.util.control.Breaks

final case class SortWith[A](outer: Pat[Pat[A]], it: It[(A, A)], lt: Pat[Boolean])
  extends Pattern[Pat[A]] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, Pat[A]] =
    new StreamImpl[Tx](tx)

  def transform(t: Transform): Pat[Pat[A]] = ???

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends Stream[Tx, Pat[A]] {

    @transient final private[this] lazy val ref = new AnyRef

    private def mkItStream(implicit tx: Tx) = {
      val res = new SortWithItStream[Tx, A](tx)
      ctx.addStream(ref, res)
      res
    }

    ctx.provideOuterStream[(A, A)](it.token, mkItStream(_))(tx0)

    private[this] val outerStream: Stream[Tx, Pat[A]]  = outer.expand(ctx, tx0)
    private[this] val ltStream   : Stream[Tx, Boolean] = lt   .expand(ctx, tx0)

    private[this] val _valid      = ctx.newVar(false)

    private[this] val sortedIt    = ctx.newVar[Stream[Tx, Pat[A]]](null)
    private[this] val _hasSorted  = ctx.newVar(false)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()      = true
        _hasSorted()  = false
        perform()
      }

    private def perform()(implicit tx: Tx): Unit = {
      val vec: Vector[Vector[A]] = outerStream.map(_.expand.toVector).toVector
      val itStreams = ctx.getStreams(ref)
      Breaks.breakable {
        val sorted = vec.sortWith { (x, y) =>
          itStreams.foreach {
            case m: SortWithItStream[Tx, A] => m.advance(x, y)
          }
          ltStream.reset()
          if (ltStream.hasNext) {
            ltStream.next()
          } else {
            Breaks.break()
          }
        }
        _hasSorted() = true
        sortedIt() = Stream(sorted.map(xs => Pat(xs: _*) /* Stream(xs: _*) */): _*)
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

    def next()(implicit tx: Tx): Pat[A] = {
      if (!hasNext) Stream.exhausted()
      sortedIt().next()
    }
  }
}
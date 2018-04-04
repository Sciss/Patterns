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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.impl.SortWithItStream

import scala.util.control.Breaks

final case class SortWith[A](outer: Pat[Pat[A]], it: It[(A, A)], lt: Pat[Boolean])
  extends Pattern[Pat[A]] {

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] =
    new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Pat[A]] = {
    val outerT  = t(outer)
    val ltT     = t(lt)
    if (outerT.eq(outer) && ltT.eq(lt)) this else {
      val (itT, ltT1) = it.replaceIn(ltT)
      copy(outer = outerT, it = itT, lt = ltT1)
    }
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S])
    extends Stream[S, Pat[A]] {

    @transient final private[this] lazy val ref = new AnyRef

    private[this] val id          = tx0.newId()
    private[this] val _valid      = tx0.newBooleanVar(id, false)
    private[this] val sortedIt    = ??? : S#Var[Stream[S, Pat[A]]] // ctx.newVar[Stream[S, Pat[A]]](null)(tx0)
    private[this] val _hasSorted  = tx0.newBooleanVar(id, false)

    private def mkItStream(implicit tx: S#Tx) = {
      val res = new SortWithItStream[S, A](tx)
      ctx.addStream(ref, res)
      res
    }

    ctx.provideOuterStream[(A, A)](it.token, mkItStream(_))(tx0)

    private[this] val outerStream: Stream[S, Pat[A]]  = outer.expand(ctx, tx0)
    private[this] val ltStream   : Stream[S, Boolean] = lt   .expand(ctx, tx0)

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
//      logStream("SortWith.iterator.reset()")
      _valid() = false
      val itStreams = ctx.getStreams(ref)
      itStreams.foreach {
        case m: SortWithItStream[S, A] => m.reset()
      }
      outerStream .reset()
      ltStream    .reset()
    }

    private def validate()(implicit tx: S#Tx): Unit =
      if (!_valid()) {
        _valid()      = true
        _hasSorted()  = false
        perform()
      }

    private def perform()(implicit tx: S#Tx): Unit = {
      val vec: Vector[Vector[A]] = outerStream.map(_.expand.toVector).toVector
      val itStreams = ctx.getStreams(ref)
      Breaks.breakable {
        val sorted = vec.sortWith { (x, y) =>
          itStreams.foreach {
            case m: SortWithItStream[S, A] => m.advance(x, y)
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

    def hasNext(implicit tx: S#Tx): Boolean = {
      validate()
      _hasSorted() && sortedIt().hasNext
    }

    def next()(implicit tx: S#Tx): Pat[A] = {
      if (!hasNext) Stream.exhausted()
      sortedIt().next()
    }
  }
}
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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.stream.MapItStream
import de.sciss.serial.DataOutput

final case class FlatMap[A1, A] private[patterns](outer: Pat[Pat[A1]], it: It[A1], inner: Pat[A], innerLevel: Int)
  extends Pattern[A] {

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    logStream("FlatMap.iterator")
    new StreamImpl(tx)
  }

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val outerT  = t(outer)
    val innerT  = t(inner)
    if (outerT.eq(outer) && innerT.eq(inner)) this else {
      val (itT, innerT1) = it.replaceIn(innerT)
      copy(outer = outerT, it = itT, inner = innerT1)
    }
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, A] {
    @transient final private[this] lazy val ref = new AnyRef

    protected def typeId: Int = ???

    protected def writeData(out: DataOutput): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = ???

    private def mkItStream(implicit tx: S#Tx) = {
      val res = new MapItStream(outer, tx)
      ctx.addStream(ref, res)
      res
    }

    ctx.provideOuterStream(it.token, mkItStream(_))(tx0)

    private[this] val innerStream: Stream[S, A] = inner.expand(ctx, tx0)

    // because `inner` is not guaranteed to depend on `It`, we must
    // pro-active create one instance of the it-stream which is used
    // as an additional constraint to determine `hasNext`!
    private[this] val itStream      = mkItStream(tx0)

    def reset()(implicit tx: S#Tx): Unit = {
      logStream("FlatMap.iterator.reset()")
      ctx.getStreams(ref).foreach {
        case m: MapItStream[S, _] => m.resetOuter()
        // case _ =>
      }
      innerStream.reset()
    }

//      ctx.getStreams(ref).foreach {
//        case m: MapItStream[S, _] => m.resetOuter()
//        // case _ =>
//      }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
      itStream.hasNext && innerStream.hasNext

    private def advance()(implicit tx: S#Tx): Unit = {
      logStream("FlatMap.iterator.advance()")
      ctx.getStreams(ref).foreach {
        case m: MapItStream[S, _] => m.advance()
        // case _ =>
      }
      innerStream.reset()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = innerStream.next()
      logStream(s"FlatMap.iterator.next() = $res; innerStream.hasNext = ${innerStream.hasNext}; itStream.hasNext = ${itStream.hasNext}")
      if (!innerStream.hasNext && itStream.hasNext) advance()
      res
    }
  }
}

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

import de.sciss.patterns.graph.impl.MapItStream

final case class FlatMap[A1, A] private[patterns](outer: Pat[Pat[A1]], it: It[A1], inner: Pat[A], innerLevel: Int)
  extends Pattern[A] {

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = {
    logStream("FlatMap.iterator")
    new StreamImpl(tx)
  }

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val outerT  = t(outer)
    val innerT  = t(inner)
    if (outerT.eq(outer) && innerT.eq(inner)) this else {
      val (itT, innerT1) = it.replaceIn(innerT)
      copy(outer = outerT, it = itT, inner = innerT1)
    }
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    @transient final private[this] lazy val ref = new AnyRef

    private def mkItStream(implicit tx: Tx) = {
      val res = new MapItStream(outer, tx)
      ctx.addStream(ref, res)
      res
    }

    ctx.provideOuterStream(it.token, mkItStream(_))(tx0)

    private[this] val innerStream: Stream[Tx, A] = inner.expand(ctx, tx0)

    // because `inner` is not guaranteed to depend on `It`, we must
    // pro-active create one instance of the it-stream which is used
    // as an additional constraint to determine `hasNext`!
    private[this] val itStream      = mkItStream(tx0)

    def reset(level: Int)(implicit tx: Tx): Unit = {
      logStream("FlatMap.iterator.reset()")
      ctx.getStreams(ref).foreach {
        case m: MapItStream[Tx, _] => m.resetOuter(level)
        // case _ =>
      }
      innerStream.reset(level)
    }

//      ctx.getStreams(ref).foreach {
//        case m: MapItStream[Tx, _] => m.resetOuter()
//        // case _ =>
//      }

    def hasNext(implicit tx: Tx): Boolean =
      itStream.hasNext && innerStream.hasNext

    private def advance()(implicit tx: Tx): Unit = {
      logStream("FlatMap.iterator.advance()")
      ctx.getStreams(ref).foreach {
        case m: MapItStream[Tx, _] => m.advance()
        // case _ =>
      }
      innerStream.reset(innerLevel)
    }

    def next()(implicit tx: Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = innerStream.next()
      logStream(s"FlatMap.iterator.next() = $res; innerStream.hasNext = ${innerStream.hasNext}; itStream.hasNext = ${itStream.hasNext}")
      if (!innerStream.hasNext && itStream.hasNext) advance()
      res
    }
  }
}

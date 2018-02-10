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

import de.sciss.patterns.graph.impl.{FoldLeftCarryBuffer, FoldLeftCarryStream, MapItStream}

final case class FoldLeft[B, A](outer: Pat[Pat[B]], z: Pat[A], itIn: It[B], itCarry: It[A],
                                inner: Graph[A])
  extends Pattern[A] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] =
    new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    @transient final private[this] lazy val refIn     = new AnyRef
    @transient final private[this] lazy val refCarry  = new AnyRef

    ctx.provideOuterStream[B](itIn   .token, mkItInStream   (_))(tx0)
    ctx.provideOuterStream[A](itCarry.token, mkItCarryStream(_))(tx0)

    private[this] val buf           = new FoldLeftCarryBuffer[Tx, A](tx0)
    // because `inner` is not guaranteed to depend on `It`, we must
    // pro-active create one instance of the it-stream which is used
    // as an additional constraint to determine `hasNext`!
    private[this] val itInStream    = mkItInStream(tx0)
    private[this] val innerStream   = inner.expand(ctx, tx0)
    private[this] val zStream       = z    .expand(ctx, tx0)

    private[this] val _valid        = ctx.newVar(false)
    private[this] val _result       = ctx.newVar[Stream[Tx, A]](null)

    private def mkItInStream(implicit tx: Tx) = {
      logStream("FoldLeft.iterator.mkItInStream")
      val res = new MapItStream[Tx, B](outer, tx)
      ctx.addStream(refIn, res)
      res
    }

    private def mkItCarryStream(implicit tx: Tx) = {
      logStream("FoldLeft.iterator.mkItCarryStream")
      val res = new FoldLeftCarryStream[Tx, A](buf)
      ctx.addStream(refCarry, res)
      buf.addIt(res)
      res
    }

    private def captureZ(in: Stream[Tx, A])(implicit tx: Tx): Vector[A] = {
      val bi = Vector.newBuilder[A]
      while (in.hasNext) {
        val v = in.next()
        bi += v
      }
      bi.result()
    }

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        logStream("FoldLeft.iterator.validate()")
        _valid() = true
        val z0 = captureZ(zStream)
        buf.advance(z0)
        val itInStreams = ctx.getStreams(refIn)
        while (itInStream.hasNext && innerStream.hasNext) {
          val curr = captureZ(innerStream)
          buf.advance(curr)
          itInStreams.foreach {
            case m: MapItStream[Tx, _] => m.advance()
          }
          innerStream.reset()
        }
        _result() = Stream(buf.result: _*)
      }

    def reset()(implicit tx: Tx): Unit = {
      logStream("FoldLeft.iterator.reset()")
      _valid() = false
      buf.clear()
      ctx.getStreams(refIn).foreach {
        case m: MapItStream[Tx, _] => m.resetOuter()
      }
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _result().hasNext
    }

    def next()(implicit tx: Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = _result().next()
      logStream(s"FoldLeft.iterator.next() = $res")
      res
    }
  }
}
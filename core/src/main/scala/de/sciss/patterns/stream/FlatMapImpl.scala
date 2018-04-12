/*
 *  FlatMapImpl.scala
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
package stream

import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.FlatMap
import de.sciss.serial.{DataInput, DataOutput}

object FlatMapImpl extends StreamFactory {
  final val typeId = 0x464D6170 // "FMap"

  def expand[S <: Base[S], A1, A](pat: FlatMap[A1, A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    new StreamNew[S, A1, A](ctx, tx, outer = outer, token = it.token, inner = inner)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val outer         = Pat.read[Pat[Any]](in)
    val token         = in.readInt()

    new StreamRead[S, Any, Any](ctx, tx, in, access, outer = outer, token = token)
  }

  private final class StreamNew [S <: Base[S], A1, A](ctx0: Context[S], tx0: S#Tx,
                                                      outer       : Pat[Pat[A1]],
                                                      token       : Int,
                                                      inner       : Pat[A]
  )
    extends StreamImpl[S, A1, A](tx0, outer, token) {

    protected val innerStream : Stream[S, A]  = ctx0.withItSource(this)(inner.expand[S](ctx0, tx0))(tx0)
    protected val itStream    : Stream[S, A1] = mkItStream()(ctx0, tx0)
  }

  private final class StreamRead[S <: Base[S], A1, A](ctx0: Context[S], tx0: S#Tx, in0: DataInput, access0: S#Acc,
                                                      outer: Pat[Pat[A1]],
                                                      token: Int
                                                     )
    extends StreamImpl[S, A1, A](tx0, outer = outer, token = token) {

    protected val (innerStream: Stream[S, A], itStream: Stream[S, A1]) = {
      ctx0.withItSource(this) {
        val _1 = Stream.read[S, A ](in0, access0)(ctx0, tx0)
        val _2 = Stream.read[S, A1](in0, access0)(ctx0, tx0)
        (_1, _2)
      } (tx0)
    }
  }

  private abstract class StreamImpl[S <: Base[S], A1, A](tx0: S#Tx,
                                                          outer            : Pat[Pat[A1]],
                                                          final val token: Int
                                                        )
    extends Stream[S, A] with ItStreamSource[S, A1] {

    // ---- abstract ----

    protected val innerStream : Stream[S, A]
    // because `inner` is not guaranteed to depend on `It`, we must
    // pro-active create one instance of the it-stream which is used
    // as an additional constraint to determine `hasNext`!
    protected val itStream    : Stream[S, A1]

    // ---- impl ----

    final protected val mapItStreams = tx0.newInMemorySet[ItStream[S, A1]]

    final protected def typeId: Int = FlatMapImpl.typeId

    final protected def writeData(out: DataOutput): Unit = {
      Pat.write(outer, out)
      out.writeInt(token)
      innerStream.write(out)
      itStream   .write(out)
    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      innerStream.dispose()
      mapItStreams.foreach(_.dispose())
    }

    final def mkItStream()(implicit ctx: Context[S], tx: S#Tx): ItStream[S, A1] = {
      val res = MapItStream.expand[S, A1](outer, token = token)
      mapItStreams.add(res)
      res
    }

    final def registerItStream(stream: ItStream[S, A1])(implicit tx: S#Tx): Unit =
      mapItStreams.add(stream)

    final def reset()(implicit tx: S#Tx): Unit = {
      // $COVERAGE-OFF$
      logStream("FlatMap.iterator.reset()")
      // $COVERAGE-ON$
      mapItStreams /* ctx.getStreams(ref) */.foreach {
        case m: AdvanceItStream[S, _] => m.resetOuter()
      }
      innerStream.reset()
    }

    final def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
      ctx.withItSource(this) {
        hasNextI
      }

    private def hasNextI(implicit ctx: Context[S], tx: S#Tx): Boolean =
      itStream.hasNext && innerStream.hasNext

    private def advance()(implicit ctx: Context[S], tx: S#Tx): Unit = {
      // $COVERAGE-OFF$
      logStream("FlatMap.iterator.advance()")
      // $COVERAGE-ON$
      mapItStreams /* ctx.getStreams(ref) */.foreach {
        case m: AdvanceItStream[S, _] => m.advance()
      }
      innerStream.reset()
    }

    final def next()(implicit ctx: Context[S], tx: S#Tx): A =
      ctx.withItSource(this) {
        if (!hasNextI) Stream.exhausted()
        val res = innerStream.next()
        // $COVERAGE-OFF$
        logStream(s"FlatMap.iterator.next() = $res; innerStream.hasNext = ${innerStream.hasNext}; itStream.hasNext = ${itStream.hasNext}")
        // $COVERAGE-ON$
        if (!innerStream.hasNext && itStream.hasNext) advance()
        res
      }
  }
}

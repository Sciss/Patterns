/*
 *  FlatMapImpl.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package stream

import de.sciss.lucre.Exec
import de.sciss.patterns.graph.{FlatMap, Pat}
import de.sciss.serial.{DataInput, DataOutput}
import de.sciss.patterns.Log.{stream => logStream}

object FlatMapImpl extends StreamFactory {
  final val typeId = 0x464D6170 // "FMap"

  def expand[T <: Exec[T], A1, A](pat: FlatMap[A1, A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    new StreamNew[T, A1, A](ctx, tx, outer = outer, token = it.token, inner = inner)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val outer         = Pat.read[Pat[Any]](in)
    val token         = in.readInt()

    new StreamRead[T, Any, Any](ctx, tx, in, outer = outer, token = token)
  }

  private final class StreamNew [T <: Exec[T], A1, A](ctx0: Context[T], tx0: T,
                                                      outer       : Pat[Pat[A1]],
                                                      token       : Int,
                                                      inner       : Pat[A]
  )
    extends StreamImpl[T, A1, A](tx0, outer, token) {

    protected val innerStream : Stream[T, A]  = ctx0.withItSource(this)(inner.expand[T](ctx0, tx0))(tx0)
    protected val itStream    : Stream[T, A1] = mkItStream()(ctx0, tx0)
  }

  private final class StreamRead[T <: Exec[T], A1, A](ctx0: Context[T], tx0: T, in0: DataInput,
                                                      outer: Pat[Pat[A1]],
                                                      token: Int
                                                     )
    extends StreamImpl[T, A1, A](tx0, outer = outer, token = token) {

    protected val (innerStream: Stream[T, A], itStream: Stream[T, A1]) = {
      ctx0.withItSource(this) {
        val _1 = Stream.read[T, A ](in0)(ctx0, tx0)
        val _2 = Stream.read[T, A1](in0)(ctx0, tx0)
        (_1, _2)
      } (tx0)
    }
  }

  private final class StreamCopy[T <: Exec[T], A1, A](tx0: T,
                                                      outer       : Pat[Pat[A1]],
                                                      token       : Int,
                                                      protected val innerStream : Stream[T, A],
                                                      protected val itStream    : Stream[T, A1]
                                                     )
    extends StreamImpl[T, A1, A](tx0, outer, token)

  private abstract class StreamImpl[T <: Exec[T], A1, A](tx0: T,
                                                          outer            : Pat[Pat[A1]],
                                                          final val token: Int
                                                        )
    extends Stream[T, A] with ItStreamSource[T, A1] {

    // ---- abstract ----

    protected val innerStream : Stream[T, A]
    // because `inner` is not guaranteed to depend on `It`, we must
    // pro-active create one instance of the it-stream which is used
    // as an additional constraint to determine `hasNext`!
    protected val itStream    : Stream[T, A1]

    // ---- impl ----

    private[patterns] final def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                            (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val innerStreamOut  = c(innerStream)
      val itStreamOut     = c(itStream   )
      new StreamCopy[Out, A1, A](txOut, outer = outer, token = token,
        innerStream = innerStreamOut, itStream = itStreamOut)
    }

    final protected val mapItStreams = tx0.newInMemorySet[ItStream[T, A1]]

    final protected def typeId: Int = FlatMapImpl.typeId

    final protected def writeData(out: DataOutput): Unit = {
      Pat.write(outer, out)
      out.writeInt(token)
      innerStream.write(out)
      itStream   .write(out)
    }

    final def dispose()(implicit tx: T): Unit = {
      innerStream.dispose()
      mapItStreams.foreach(_.dispose())
    }

    final def mkItStream()(implicit ctx: Context[T], tx: T): ItStream[T, A1] = {
      val res = MapItStream.expand[T, A1](outer, token = token)
      mapItStreams.add(res)
      res
    }

    final def registerItStream(stream: ItStream[T, A1])(implicit tx: T): Unit =
      mapItStreams.add(stream)

    final def reset()(implicit tx: T): Unit = {
      // $COVERAGE-OFF$
      logStream.debug("FlatMap.iterator.reset()")
      // $COVERAGE-ON$
      mapItStreams /* ctx.getStreams(ref) */.foreach {
        case m: AdvanceItStream[T, _] => m.resetOuter()
      }
      innerStream.reset()
    }

    final def hasNext(implicit ctx: Context[T], tx: T): Boolean =
      ctx.withItSource(this) {
        hasNextI
      }

    private def hasNextI(implicit ctx: Context[T], tx: T): Boolean =
      itStream.hasNext && innerStream.hasNext

    private def advance()(implicit ctx: Context[T], tx: T): Unit = {
      // $COVERAGE-OFF$
      logStream.debug("FlatMap.iterator.advance()")
      // $COVERAGE-ON$
      mapItStreams /* ctx.getStreams(ref) */.foreach {
        case m: AdvanceItStream[T, _] => m.advance()
      }
      innerStream.reset()
    }

    final def next()(implicit ctx: Context[T], tx: T): A =
      ctx.withItSource(this) {
        if (!hasNextI) Stream.exhausted()
        val res = innerStream.next()
        // $COVERAGE-OFF$
        logStream.debug(s"FlatMap.iterator.next() = $res; innerStream.hasNext = ${innerStream.hasNext}; itStream.hasNext = ${itStream.hasNext}")
        // $COVERAGE-ON$
        if (!innerStream.hasNext && itStream.hasNext) advance()
        res
      }
  }
}

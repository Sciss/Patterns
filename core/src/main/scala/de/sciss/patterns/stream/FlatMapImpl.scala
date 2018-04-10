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

import de.sciss.lucre.stm.{Base, RefSet}
import de.sciss.patterns.graph.FlatMap
import de.sciss.serial.{DataInput, DataOutput}

object FlatMapImpl extends StreamFactory {
  final val typeId = 0x464D6170 // "FMap"

  def expand[S <: Base[S], A1, A](pat: FlatMap[A1, A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val mapItStreams = tx.newInMemorySet[Stream[S, A1]]
    new StreamNew[S, A1, A](ctx, tx, outer = outer, tokenId = it.token, inner = inner, mapItStreams = mapItStreams)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val mapItStreams  = tx.newInMemorySet[Stream[S, Any]]
    val outer         = Pat.serializer[Pat[Any]].read(in)
    val tokenId       = in.readInt()
    val innerStream   = Stream.read[S, Any](in, access)
    val itStream      = Stream.read[S, Any](in, access) // XXX TODO --- do we need to 'ping' itStream?

    new StreamRead[S, Any, Any](outer = outer, tokenId = tokenId, innerStream = innerStream,
      itStream = itStream, mapItStreams = mapItStreams)
  }

  private final class StreamNew [S <: Base[S], A1, A](ctx0: Context[S], tx0: S#Tx,
                                                      outer: Pat[Pat[A1]],
                                                      tokenId: Int,
                                                      inner: Pat[A],
                                                      mapItStreams: RefSet[S, Stream[S, A1]]
  )
    extends StreamImpl[S, A1, A](outer, tokenId, mapItStreams) {

    protected val innerStream : Stream[S, A]  = ctx0.withItSource(tokenId, this)(inner.expand[S](ctx0, tx0))(tx0)
    protected val itStream    : Stream[S, A1] = mkItStream()(ctx0, tx0)
  }

  private final class StreamRead[S <: Base[S], A1, A](outer: Pat[Pat[A1]],
                                                      tokenId: Int,
                                                      protected val innerStream: Stream[S, A],
                                                      protected val itStream   : Stream[S, A1],
                                                      mapItStreams: RefSet[S, Stream[S, A1]]
                                                     )
    extends StreamImpl[S, A1, A](outer, tokenId, mapItStreams)

  private abstract class StreamImpl[S <: Base[S], A1, A](
                                                          outer: Pat[Pat[A1]],
                                                          tokenId: Int,
                                                          mapItStreams: RefSet[S, Stream[S, A1]]
                                                        )
    extends Stream[S, A] with ItStreamSource[S, A1] {

    // ---- abstract ----

    protected val innerStream : Stream[S, A]
    // because `inner` is not guaranteed to depend on `It`, we must
    // pro-active create one instance of the it-stream which is used
    // as an additional constraint to determine `hasNext`!
    protected val itStream    : Stream[S, A1]

    // ---- impl ----

    final protected def typeId: Int = FlatMapImpl.typeId

    final protected def writeData(out: DataOutput): Unit = {
      Pat.serializer[Pat[A1]].write(outer, out)
      out.writeInt(tokenId)
      innerStream.write(out)
      itStream   .write(out)
    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      innerStream.dispose()
      mapItStreams.foreach(_.dispose())
    }

    final def mkItStream()(implicit ctx: Context[S], tx: S#Tx): Stream[S, A1] = {
      val res = new MapItStream[S, A1](outer, tx)
      mapItStreams.add(res)
      res
    }

    final def pingFromIt(stream: Stream[S, A1])(implicit tx: S#Tx): Unit =
      mapItStreams.add(stream)

    final def reset()(implicit tx: S#Tx): Unit = {  // XXX TODO --- do we need ctx.use here?
      logStream("FlatMap.iterator.reset()")
      mapItStreams /* ctx.getStreams(ref) */.foreach {
        case m: MapItStream[S, _] => m.resetOuter()
      }
      innerStream.reset()
    }

    final def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
      ctx.withItSource(tokenId, this) {
        hasNextI
      }

    private def hasNextI(implicit ctx: Context[S], tx: S#Tx): Boolean =
      itStream.hasNext && innerStream.hasNext

    private def advance()(implicit tx: S#Tx): Unit = {
      logStream("FlatMap.iterator.advance()")
      mapItStreams /* ctx.getStreams(ref) */.foreach {
        case m: MapItStream[S, _] => m.advance()
      }
      innerStream.reset()
    }

    final def next()(implicit ctx: Context[S], tx: S#Tx): A =
      ctx.withItSource(tokenId, this) {
        if (!hasNextI) Stream.exhausted()
        val res = innerStream.next()
        logStream(s"FlatMap.iterator.next() = $res; innerStream.hasNext = ${innerStream.hasNext}; itStream.hasNext = ${itStream.hasNext}")
        if (!innerStream.hasNext && itStream.hasNext) advance()
        res
      }
  }
}
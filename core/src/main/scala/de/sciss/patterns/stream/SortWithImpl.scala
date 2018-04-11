/*
 *  SortWithImpl.scala
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
import de.sciss.patterns.graph.SortWith
import de.sciss.serial.{DataInput, DataOutput}

import scala.util.control.Breaks

object SortWithImpl extends StreamFactory {
  final val typeId = 0x536F7257 // "SorW"

  def expand[S <: Base[S], A](pat: SortWith[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] = {
    import pat._
    val mapItStreams  = tx.newInMemorySet[Stream[S, (A, A)]]
    val id            = tx.newId()
    val outerStream   = outer.expand[S]
    val sortedStream  = tx.newVar[Stream[S, Pat[A]]](id, null)
    val hasSorted     = tx.newBooleanVar(id, false)
    val valid         = tx.newBooleanVar(id, false)

    new StreamNew[S, A](ctx, tx, id = id, outerStream = outerStream, tokenId = it.token, sortedStream = sortedStream,
      hasSorted = hasSorted, valid = valid, lt = lt, mapItStreams = mapItStreams)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val mapItStreams  = tx.newInMemorySet[Stream[S, (Any, Any)]]
    val id            = tx.readId(in, access)
    val outerStream   = Stream.read[S, Pat[Any]](in, access)
    val tokenId       = in.readInt()
    val sortedStream  = tx.readVar[Stream[S, Pat[Any]]](id, in)
    val hasSorted     = tx.readBooleanVar(id, in)
    val valid         = tx.readBooleanVar(id, in)
    val ltStream      = Stream.read[S, Boolean](in, access)

    new StreamRead[S, Any](id = id, outerStream = outerStream, tokenId = tokenId, sortedStream = sortedStream,
      hasSorted = hasSorted, valid = valid, ltStream = ltStream, mapItStreams = mapItStreams)
  }

  private final class StreamNew [S <: Base[S], A](ctx0: Context[S], tx0: S#Tx,
                                                  id          : S#Id,
                                                  outerStream : Stream[S, Pat[A]],
                                                  tokenId     : Int,
                                                  sortedStream: S#Var[Stream[S, Pat[A]]],
                                                  hasSorted   : S#Var[Boolean],
                                                  valid       : S#Var[Boolean],
                                                  lt          : Pat[Boolean],
                                                  mapItStreams: RefSet[S, Stream[S, (A, A)]]
                                                 )
    extends StreamImpl[S, A](id, outerStream = outerStream, tokenId = tokenId, sortedStream = sortedStream,
      hasSorted = hasSorted, valid = valid, mapItStreams = mapItStreams) {

    protected val ltStream : Stream[S, Boolean] = ctx0.withItSource(tokenId, this)(lt.expand[S](ctx0, tx0))(tx0)
  }

  private final class StreamRead[S <: Base[S], A](id          : S#Id,
                                                  outerStream : Stream[S, Pat[A]],
                                                  tokenId     : Int,
                                                  sortedStream: S#Var[Stream[S, Pat[A]]],
                                                  hasSorted   : S#Var[Boolean],
                                                  valid       : S#Var[Boolean],
                                                  protected val ltStream: Stream[S, Boolean],
                                                  mapItStreams: RefSet[S, Stream[S, (A, A)]]
                                                 )
    extends StreamImpl[S, A](id, outerStream = outerStream, tokenId = tokenId, sortedStream = sortedStream,
      hasSorted = hasSorted, valid = valid, mapItStreams = mapItStreams)

  private abstract class StreamImpl[S <: Base[S], A](
                                                      id          : S#Id,
                                                      outerStream : Stream[S, Pat[A]],
                                                      tokenId     : Int,
                                                      sortedStream: S#Var[Stream[S, Pat[A]]],
                                                      hasSorted   : S#Var[Boolean],
                                                      valid       : S#Var[Boolean],
                                                      mapItStreams: RefSet[S, Stream[S, (A, A)]]
                                                    )
    extends Stream[S, Pat[A]] with ItStreamSource[S, (A, A)] {

    // ---- abstract ----

    protected val ltStream : Stream[S, Boolean]

    // ---- impl ----

    final protected def typeId: Int = SortWithImpl.typeId

    final protected def writeData(out: DataOutput): Unit = {
      id          .write(out)
      outerStream .write(out)
      ltStream    .write(out)
      out.writeInt(tokenId)
      sortedStream.write(out)
      hasSorted   .write(out)
      valid       .write(out)
    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      id          .dispose()
      outerStream .dispose()
      ltStream    .dispose()
      sortedStream.dispose()
      hasSorted   .dispose()
      valid       .dispose()
      mapItStreams.foreach(_.dispose())
    }

    final def mkItStream()(implicit ctx: Context[S], tx: S#Tx): Stream[S, (A, A)] = {
      val res = new SortWithItStream[S, A](tx)
      mapItStreams.add(res)
      res
    }

    final def pingFromIt(stream: Stream[S, (A, A)])(implicit tx: S#Tx): Unit =
      mapItStreams.add(stream)

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
//      val itStreams = ctx.getStreams(ref)
      mapItStreams /* itStreams */.foreach {
        case m: SortWithItStream[S, A] => m.reset()
      }
      outerStream .reset()
      ltStream    .reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      hasSorted()  = false
      perform()
    }

    private def perform()(implicit ctx: Context[S], tx: S#Tx): Unit = {
      val vec: Vector[Vector[A]] = outerStream.toIterator.map(_.expand.toVector).toVector
      //      val itStreams = ctx.getStreams(ref)
      Breaks.breakable {
        val sorted = vec.sortWith { (x, y) =>
          mapItStreams /* itStreams */.foreach {
            case m: SortWithItStream[S, A] => m.advance(x, y)
          }
          ltStream.reset()
          if (ltStream.hasNext) {
            ltStream.next()
          } else {
            Breaks.break()
          }
        }
        hasSorted() = true
        sortedStream() = Stream(sorted.map(xs => Pat(xs: _*)): _*)
      }
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
      ctx.withItSource(tokenId, this) {
        hasNextI
      }

    private def hasNextI(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      hasSorted() && sortedStream().hasNext
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): Pat[A] =
      ctx.withItSource(tokenId, this) {
        if (!hasNextI) Stream.exhausted()
        sortedStream().next()
      }
  }
}

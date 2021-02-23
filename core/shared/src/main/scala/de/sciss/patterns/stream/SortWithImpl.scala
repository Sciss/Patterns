/*
 *  SortWithImpl.scala
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

import de.sciss.lucre.{Exec, Ident, Var}
import de.sciss.patterns.graph.{Pat, SortWith}
import de.sciss.serial.{DataInput, DataOutput}

import scala.util.control.Breaks

object SortWithImpl extends StreamFactory {
  final val typeId = 0x536F7257 // "SorW"

  def expand[T <: Exec[T], A](pat: SortWith[A])(implicit ctx: Context[T], tx: T): Stream[T, Pat[A]] = {
    import pat._
    val id            = tx.newId()
    val outerStream   = outer.expand[T]
    val sortedStream  = id.newVar[Stream[T, Pat[A]]](null)
    val hasSorted     = id.newBooleanVar(false)
    val valid         = id.newBooleanVar(false)

    new StreamNew[T, A](ctx, tx, id = id, outerStream = outerStream, token = it.token,
      sortedStream = sortedStream, hasSorted = hasSorted, valid = valid, lt = lt)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id            = tx.readId(in)
    val outerStream   = Stream.read[T, Pat[Any]](in)
    val tokenId       = in.readInt()
    val sortedStream  = id.readVar[Stream[T, Pat[Any]]](in)
    val hasSorted     = id.readBooleanVar(in)
    val valid         = id.readBooleanVar(in)

    new StreamRead[T, Any](ctx, tx, in, id = id, outerStream = outerStream, token = tokenId,
      sortedStream = sortedStream, hasSorted = hasSorted, valid = valid)
  }

  private final class StreamCopy[T <: Exec[T], A](tx0: T,
                                                  id          : Ident[T],
                                                  outerStream : Stream[T, Pat[A]],
                                                  token       : Int,
                                                  sortedStream: Var[T, Stream[T, Pat[A]]],
                                                  hasSorted   : Var[T, Boolean],
                                                  valid       : Var[T, Boolean],
                                                  protected val ltStream: Stream[T, Boolean]
                                                 )
    extends StreamImpl[T, A](tx0, id, outerStream = outerStream, token = token, sortedStream = sortedStream,
      hasSorted = hasSorted, valid = valid)

  private final class StreamNew [T <: Exec[T], A](ctx0: Context[T], tx0: T,
                                                  id          : Ident[T],
                                                  outerStream : Stream[T, Pat[A]],
                                                  token       : Int,
                                                  sortedStream: Var[T, Stream[T, Pat[A]]],
                                                  hasSorted   : Var[T, Boolean],
                                                  valid       : Var[T, Boolean],
                                                  lt          : Pat[Boolean]
                                                 )
    extends StreamImpl[T, A](tx0, id, outerStream = outerStream, token = token, sortedStream = sortedStream,
      hasSorted = hasSorted, valid = valid) {

    protected val ltStream: Stream[T, Boolean] = ctx0.withItSource(this)(lt.expand[T](ctx0, tx0))(tx0)
  }

  private final class StreamRead[T <: Exec[T], A](ctx0: Context[T], tx0: T, in0: DataInput,
                                                  id          : Ident[T],
                                                  outerStream : Stream[T, Pat[A]],
                                                  token       : Int,
                                                  sortedStream: Var[T, Stream[T, Pat[A]]],
                                                  hasSorted   : Var[T, Boolean],
                                                  valid       : Var[T, Boolean]
                                                 )
    extends StreamImpl[T, A](tx0, id, outerStream = outerStream, token = token, sortedStream = sortedStream,
      hasSorted = hasSorted, valid = valid) {

    protected val ltStream: Stream[T, Boolean] =
      ctx0.withItSource(this)(Stream.read[T, Boolean](in0)(ctx0, tx0))(tx0)
  }

  private abstract class StreamImpl[T <: Exec[T], A](tx0: T,
                                                      id               : Ident[T],
                                                      outerStream      : Stream[T, Pat[A]],
                                                      final val token: Int,
                                                      sortedStream     : Var[T, Stream[T, Pat[A]]],
                                                      hasSorted        : Var[T, Boolean],
                                                      valid            : Var[T, Boolean]
                                                    )
    extends Stream[T, Pat[A]] with ItStreamSource[T, (A, A)] {

    // ---- abstract ----

    protected val ltStream : Stream[T, Boolean]

    // ---- impl ----

    private[patterns] final def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                            (implicit tx: T, txOut: Out): Stream[Out, Pat[A]] = {
      val idOut           = txOut.newId()
      val outerStreamOut  = c(outerStream)
      val sortedStreamOut = c.copyVar(idOut, sortedStream)
      val hasSortedOut    = idOut.newBooleanVar(hasSorted())
      val validOut        = idOut.newBooleanVar(valid())
      val ltStreamOut     = c(ltStream)

      new StreamCopy[Out, A](txOut, id = idOut, outerStream = outerStreamOut, token = token,
        sortedStream = sortedStreamOut, hasSorted = hasSortedOut, valid = validOut, ltStream = ltStreamOut)
    }

    final protected val mapItStreams = tx0.newInMemorySet[Stream[T, (A, A)]]

    final protected def typeId: Int = SortWithImpl.typeId

    final protected def writeData(out: DataOutput): Unit = {
      id          .write(out)
      outerStream .write(out)
      out.writeInt(token)
      sortedStream.write(out)
      hasSorted   .write(out)
      valid       .write(out)
      ltStream    .write(out)
    }

    final def dispose()(implicit tx: T): Unit = {
      id          .dispose()
      outerStream .dispose()
      sortedStream.dispose()
      hasSorted   .dispose()
      valid       .dispose()
      ltStream    .dispose()
      mapItStreams.foreach(_.dispose())
    }

    final def mkItStream()(implicit ctx: Context[T], tx: T): ItStream[T, (A, A)] = {
      val res = SortWithItStream.expand[T, A](token)
      mapItStreams.add(res)
      res
    }

    final def registerItStream(stream: ItStream[T, (A, A)])(implicit tx: T): Unit =
      mapItStreams.add(stream)

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
//      val itStreams = ctx.getStreams(ref)
      mapItStreams /* itStreams */.foreach {
        case m: SortWithItStream[T, A] => m.reset()
      }
      outerStream .reset()
      ltStream    .reset()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      hasSorted()  = false
      perform()
    }

    private def perform()(implicit ctx: Context[T], tx: T): Unit = {
      val vec: Vector[Vector[A]] = outerStream.toIterator.map(_.expand.toVector).toVector
      //      val itStreams = ctx.getStreams(ref)
      Breaks.breakable {
        val sorted = vec.sortWith { (x, y) =>
          mapItStreams /* itStreams */.foreach {
            case m: SortWithItStream[T, A] => m.advance(x, y)
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

    def hasNext(implicit ctx: Context[T], tx: T): Boolean =
      ctx.withItSource(this) {
        hasNextI
      }

    private def hasNextI(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      hasSorted() && sortedStream().hasNext
    }

    def next()(implicit ctx: Context[T], tx: T): Pat[A] =
      ctx.withItSource(this) {
        if (!hasNextI) Stream.exhausted()
        sortedStream().next()
      }
  }
}

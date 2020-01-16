/*
 *  SortWithImpl.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package stream

import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.SortWith
import de.sciss.serial.{DataInput, DataOutput}

import scala.util.control.Breaks

object SortWithImpl extends StreamFactory {
  final val typeId = 0x536F7257 // "SorW"

  def expand[S <: Base[S], A](pat: SortWith[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] = {
    import pat._
    val id            = tx.newId()
    val outerStream   = outer.expand[S]
    val sortedStream  = tx.newVar[Stream[S, Pat[A]]](id, null)
    val hasSorted     = tx.newBooleanVar(id, false)
    val valid         = tx.newBooleanVar(id, false)

    new StreamNew[S, A](ctx, tx, id = id, outerStream = outerStream, token = it.token,
      sortedStream = sortedStream, hasSorted = hasSorted, valid = valid, lt = lt)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id            = tx.readId(in, access)
    val outerStream   = Stream.read[S, Pat[Any]](in, access)
    val tokenId       = in.readInt()
    val sortedStream  = tx.readVar[Stream[S, Pat[Any]]](id, in)
    val hasSorted     = tx.readBooleanVar(id, in)
    val valid         = tx.readBooleanVar(id, in)

    new StreamRead[S, Any](ctx, tx, in, access, id = id, outerStream = outerStream, token = tokenId,
      sortedStream = sortedStream, hasSorted = hasSorted, valid = valid)
  }

  private final class StreamCopy[S <: Base[S], A](tx0: S#Tx,
                                                  id          : S#Id,
                                                  outerStream : Stream[S, Pat[A]],
                                                  token       : Int,
                                                  sortedStream: S#Var[Stream[S, Pat[A]]],
                                                  hasSorted   : S#Var[Boolean],
                                                  valid       : S#Var[Boolean],
                                                  protected val ltStream: Stream[S, Boolean]
                                                 )
    extends StreamImpl[S, A](tx0, id, outerStream = outerStream, token = token, sortedStream = sortedStream,
      hasSorted = hasSorted, valid = valid)

  private final class StreamNew [S <: Base[S], A](ctx0: Context[S], tx0: S#Tx,
                                                  id          : S#Id,
                                                  outerStream : Stream[S, Pat[A]],
                                                  token       : Int,
                                                  sortedStream: S#Var[Stream[S, Pat[A]]],
                                                  hasSorted   : S#Var[Boolean],
                                                  valid       : S#Var[Boolean],
                                                  lt          : Pat[Boolean]
                                                 )
    extends StreamImpl[S, A](tx0, id, outerStream = outerStream, token = token, sortedStream = sortedStream,
      hasSorted = hasSorted, valid = valid) {

    protected val ltStream: Stream[S, Boolean] = ctx0.withItSource(this)(lt.expand[S](ctx0, tx0))(tx0)
  }

  private final class StreamRead[S <: Base[S], A](ctx0: Context[S], tx0: S#Tx, in0: DataInput, access0: S#Acc,
                                                  id          : S#Id,
                                                  outerStream : Stream[S, Pat[A]],
                                                  token       : Int,
                                                  sortedStream: S#Var[Stream[S, Pat[A]]],
                                                  hasSorted   : S#Var[Boolean],
                                                  valid       : S#Var[Boolean]
                                                 )
    extends StreamImpl[S, A](tx0, id, outerStream = outerStream, token = token, sortedStream = sortedStream,
      hasSorted = hasSorted, valid = valid) {

    protected val ltStream: Stream[S, Boolean] =
      ctx0.withItSource(this)(Stream.read[S, Boolean](in0, access0)(ctx0, tx0))(tx0)
  }

  private abstract class StreamImpl[S <: Base[S], A](tx0: S#Tx,
                                                      id               : S#Id,
                                                      outerStream      : Stream[S, Pat[A]],
                                                      final val token: Int,
                                                      sortedStream     : S#Var[Stream[S, Pat[A]]],
                                                      hasSorted        : S#Var[Boolean],
                                                      valid            : S#Var[Boolean]
                                                    )
    extends Stream[S, Pat[A]] with ItStreamSource[S, (A, A)] {

    // ---- abstract ----

    protected val ltStream : Stream[S, Boolean]

    // ---- impl ----

    private[patterns] def copyStream[Out <: Base[Out]]()(implicit tx: S#Tx, txOut: Out#Tx,
                                                         ctx: Context[Out]): Stream[Out, Pat[A]] = {
      val idOut           = txOut.newId()
      val outerStreamOut  = outerStream.copyStream[Out]()
      val sortedStreamOut = {
        val s = sortedStream()
        val sOut = if (s == null) null else s.copyStream[Out]()
        txOut.newVar[Stream[Out, Pat[A]]](idOut, sOut)
      }
      val hasSortedOut    = txOut.newBooleanVar(idOut, hasSorted())
      val validOut        = txOut.newBooleanVar(idOut, valid())
      val ltStreamOut     = ltStream.copyStream[Out]()

      new StreamCopy[Out, A](txOut, id = idOut, outerStream = outerStreamOut, token = token,
        sortedStream = sortedStreamOut, hasSorted = hasSortedOut, valid = validOut, ltStream = ltStreamOut)
    }

    final protected val mapItStreams = tx0.newInMemorySet[Stream[S, (A, A)]]

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

    final def dispose()(implicit tx: S#Tx): Unit = {
      id          .dispose()
      outerStream .dispose()
      sortedStream.dispose()
      hasSorted   .dispose()
      valid       .dispose()
      ltStream    .dispose()
      mapItStreams.foreach(_.dispose())
    }

    final def mkItStream()(implicit ctx: Context[S], tx: S#Tx): ItStream[S, (A, A)] = {
      val res = SortWithItStream.expand[S, A](token)
      mapItStreams.add(res)
      res
    }

    final def registerItStream(stream: ItStream[S, (A, A)])(implicit tx: S#Tx): Unit =
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
      ctx.withItSource(this) {
        hasNextI
      }

    private def hasNextI(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      hasSorted() && sortedStream().hasNext
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): Pat[A] =
      ctx.withItSource(this) {
        if (!hasNextI) Stream.exhausted()
        sortedStream().next()
      }
  }
}

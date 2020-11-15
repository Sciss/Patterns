/*
 *  PatMapImpl.scala
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

import de.sciss.lucre.{Exec, Ident, Var}
import de.sciss.patterns.graph.{Pat, PatMap}
import de.sciss.serial.{DataInput, DataOutput}
import de.sciss.patterns.Log.{stream => logStream}

object PatMapImpl extends StreamFactory {
  final val typeId = 0x4D617020 // "Map "

  def expand[T <: Exec[T], A1, A](pat: PatMap[A1, A])(implicit ctx: Context[T], tx: T): Stream[T, Pat[A]] = {
    import pat._
    val id            = tx.newId()
    val mapStream     = id.newVar[Pat[A]](null)
    val _hasNext      = id.newBooleanVar(false)
    val valid         = id.newBooleanVar(false)

    new StreamNew[T, A1, A](ctx, tx, id = id, outer = outer, token = it.token, mapStream = mapStream,
      _hasNext = _hasNext, valid = valid, inner = inner)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id            = tx.readId(in)
    val outer         = Pat.format[Pat[Any]].read(in)
    val tokenId       = in.readInt()
    val mapStream     = id.readVar[Pat[Any]](in)
    val _hasNext      = id.readBooleanVar(in)
    val valid         = id.readBooleanVar(in)

    new StreamRead[T, Any, Any](ctx, tx, in, id = id, outer = outer, token = tokenId, mapStream = mapStream,
      _hasNext = _hasNext, valid = valid)
  }

  private final class StreamCopy[T <: Exec[T], A1, A](tx0: T,
                                                      id          : Ident[T],
                                                      outer       : Pat[Pat[A1]],
                                                      token       : Int,
                                                      mapStream   : Var[T, Pat[A]],
                                                      _hasNext    : Var[T, Boolean],
                                                      valid       : Var[T, Boolean],
                                                      protected val innerStream : Stream[T, A],
                                                      protected val itStream    : Stream[T, A1]
                                                     )
    extends StreamImpl[T, A1, A](tx0, id, outer = outer, token = token, mapStream = mapStream,
      _hasNext = _hasNext, valid = valid)

  private final class StreamNew [T <: Exec[T], A1, A](ctx0: Context[T], tx0: T,
                                                      id          : Ident[T],
                                                      outer       : Pat[Pat[A1]],
                                                      token       : Int,
                                                      mapStream   : Var[T, Pat[A]],
                                                      _hasNext    : Var[T, Boolean],
                                                      valid       : Var[T, Boolean],
                                                      inner       : Pat[A]
                                                     )
    extends StreamImpl[T, A1, A](tx0, id, outer = outer, token = token, mapStream = mapStream,
      _hasNext = _hasNext, valid = valid) {

    protected val innerStream : Stream[T, A]  =
      ctx0.withItSource(this)(inner.expand[T](ctx0, tx0))(tx0)

    protected val itStream    : Stream[T, A1] = mkItStream()(ctx0, tx0)
  }

  private final class StreamRead[T <: Exec[T], A1, A](ctx0: Context[T], tx0: T, in0: DataInput,
                                                      id          : Ident[T],
                                                      outer       : Pat[Pat[A1]],
                                                      token       : Int,
                                                      mapStream   : Var[T, Pat[A]],
                                                      _hasNext    : Var[T, Boolean],
                                                      valid       : Var[T, Boolean]
                                                     )
    extends StreamImpl[T, A1, A](tx0, id, outer = outer, token = token, mapStream = mapStream,
      _hasNext = _hasNext, valid = valid) {

    protected val (innerStream: Stream[T, A], itStream: Stream[T, A1]) = {
      ctx0.withItSource(this) {
        val _1 = Stream.read[T, A ](in0)(ctx0, tx0)
        val _2 = Stream.read[T, A1](in0)(ctx0, tx0)
        (_1, _2)
      } (tx0)
    }
  }

  private abstract class StreamImpl[T <: Exec[T], A1, A](tx0: T,
                                                          id               : Ident[T],
                                                          outer            : Pat[Pat[A1]],
                                                          final val token: Int,
                                                          mapStream        : Var[T, Pat[A]],
                                                          _hasNext         : Var[T, Boolean],
                                                          valid            : Var[T, Boolean]
                                                        )
    extends Stream[T, Pat[A]] with ItStreamSource[T, A1] {

    // ---- abstract ----

    protected val innerStream : Stream[T, A]
    // because `inner` is not guaranteed to depend on `It`, we must
    // pro-active create one instance of the it-stream which is used
    // as an additional constraint to determine `hasNext`!
    protected val itStream    : Stream[T, A1]

    // ---- impl ----

    private[patterns] final def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                            (implicit tx: T, txOut: Out): Stream[Out, Pat[A]] = {
      val idOut           = txOut.newId()
      val mapStreamOut    = idOut.newVar[Pat[A]](mapStream())
      val hasNextOut      = idOut.newBooleanVar(_hasNext())
      val validOut        = idOut.newBooleanVar(valid())
      val innerStreamOut  = c(innerStream)
      val itStreamOut     = c(itStream   )

      new StreamCopy[Out, A1, A](txOut, id = idOut, outer = outer, token = token, mapStream = mapStreamOut,
        _hasNext = hasNextOut, valid = validOut, innerStream = innerStreamOut, itStream = itStreamOut)
    }

    final protected val mapItStreams = tx0.newInMemorySet[ItStream[T, A1]]

    final protected def typeId: Int = PatMapImpl.typeId

    final protected def writeData(out: DataOutput): Unit = {
      id          .write(out)
      Pat.format[Pat[A1]].write(outer, out)
      out.writeInt(token)
      mapStream   .write(out)
      _hasNext    .write(out)
      valid       .write(out)
      innerStream .write(out)
      itStream    .write(out)
    }

    final def dispose()(implicit tx: T): Unit = {
      id          .dispose()
      mapStream   .dispose()
      _hasNext    .dispose()
      valid       .dispose()
      innerStream .dispose()
      mapItStreams.foreach(_.dispose())
    }

    final def mkItStream()(implicit ctx: Context[T], tx: T): ItStream[T, A1] = {
      val res = MapItStream.expand[T, A1](outer, token = token)
      mapItStreams.add(res)
      res
    }

    final def registerItStream(stream: ItStream[T, A1])(implicit tx: T): Unit =
      mapItStreams.add(stream)

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      logStream.debug("PatMap.iterator.validate()")
      buildNext()
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      mapItStreams /* ctx.getStreams(ref) */.foreach {
        case m: AdvanceItStream[T, _] => m.resetOuter()
      }
      innerStream.reset()
    }

    private def advance()(implicit ctx: Context[T], tx: T): Unit = {
      mapItStreams /* ctx.getStreams(ref) */.foreach {
        case m: AdvanceItStream[T, _] => m.advance()
      }
      innerStream.reset()
      buildNext()
    }

    private def buildNext()(implicit ctx: Context[T], tx: T): Unit = {
      val hn = itStream.hasNext // && innerStream.hasNext
      _hasNext() = hn
      if (hn) {
        val b = Vector.newBuilder[A]
        var i = 0
        // there is _no_ reasonable way to provide the
        // stream than to eagerly collect the values here,
        // because of the order of execution between inner and outer
        // `next`!
        while (innerStream.hasNext) {
          b += innerStream.next()
          i += 1
        }
        val inner   = Pat(b.result(): _*)
        mapStream() = inner
        _hasNext()  = true
      }
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean =
      ctx.withItSource(this) {
        hasNextI
      }

    private def hasNextI(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[T], tx: T): Pat[A] =
      ctx.withItSource(this) {
        if (!hasNext) Stream.exhausted()
        val res = mapStream()
        advance()
        res
      }
  }
}

/*
 *  PatMapImpl.scala
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
import de.sciss.patterns.graph.PatMap
import de.sciss.serial.{DataInput, DataOutput}

object PatMapImpl extends StreamFactory {
  final val typeId = 0x4D617020 // "Map "

  def expand[S <: Base[S], A1, A](pat: PatMap[A1, A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] = {
    import pat._
    val id            = tx.newId()
    val mapStream     = tx.newVar[Pat[A]](id, null)
    val _hasNext      = tx.newBooleanVar(id, false)
    val valid         = tx.newBooleanVar(id, false)

    new StreamNew[S, A1, A](ctx, tx, id = id, outer = outer, tokenId = it.token, mapStream = mapStream,
      _hasNext = _hasNext, valid = valid, inner = inner)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id            = tx.readId(in, access)
    val outer         = Pat.serializer[Pat[Any]].read(in)
    val tokenId       = in.readInt()
    val mapStream     = tx.readVar[Pat[Any]](id, in)
    val _hasNext      = tx.readBooleanVar(id, in)
    val valid         = tx.readBooleanVar(id, in)

    new StreamRead[S, Any, Any](ctx, tx, in, access, id = id, outer = outer, tokenId = tokenId, mapStream = mapStream,
      _hasNext = _hasNext, valid = valid)
  }

  private final class StreamNew [S <: Base[S], A1, A](ctx0: Context[S], tx0: S#Tx,
                                                      id          : S#Id,
                                                      outer       : Pat[Pat[A1]],
                                                      tokenId     : Int,
                                                      mapStream   : S#Var[Pat[A]],
                                                      _hasNext    : S#Var[Boolean],
                                                      valid       : S#Var[Boolean],
                                                      inner       : Pat[A]
                                                     )
    extends StreamImpl[S, A1, A](tx0, id, outer = outer, token = tokenId, mapStream = mapStream,
      _hasNext = _hasNext, valid = valid) {

    protected val innerStream : Stream[S, A]  =
      ctx0.withItSource(this)(inner.expand[S](ctx0, tx0))(tx0)

    protected val itStream    : Stream[S, A1] = mkItStream()(ctx0, tx0)
  }

  private final class StreamRead[S <: Base[S], A1, A](ctx0: Context[S], tx0: S#Tx, in0: DataInput, access0: S#Acc,
                                                      id          : S#Id,
                                                      outer       : Pat[Pat[A1]],
                                                      tokenId     : Int,
                                                      mapStream   : S#Var[Pat[A]],
                                                      _hasNext    : S#Var[Boolean],
                                                      valid       : S#Var[Boolean]
                                                     )
    extends StreamImpl[S, A1, A](tx0, id, outer = outer, token = tokenId, mapStream = mapStream,
      _hasNext = _hasNext, valid = valid) {

    protected val (innerStream: Stream[S, A], itStream: Stream[S, A1]) = {
      ctx0.withItSource(this) {
        val _1 = Stream.read[S, A ](in0, access0)(ctx0, tx0)
        val _2 = Stream.read[S, A1](in0, access0)(ctx0, tx0)
        (_1, _2)
      } (tx0)
    }
  }

  private abstract class StreamImpl[S <: Base[S], A1, A](tx0: S#Tx,
                                                          id               : S#Id,
                                                          outer            : Pat[Pat[A1]],
                                                          final val token: Int,
                                                          mapStream        : S#Var[Pat[A]],
                                                          _hasNext         : S#Var[Boolean],
                                                          valid            : S#Var[Boolean]
                                                        )
    extends Stream[S, Pat[A]] with ItStreamSource[S, A1] {

    // ---- abstract ----

    protected val innerStream : Stream[S, A]
    // because `inner` is not guaranteed to depend on `It`, we must
    // pro-active create one instance of the it-stream which is used
    // as an additional constraint to determine `hasNext`!
    protected val itStream    : Stream[S, A1]

    // ---- impl ----

    final protected val mapItStreams = tx0.newInMemorySet[ItStream[S, A1]]

    final protected def typeId: Int = PatMapImpl.typeId

    final protected def writeData(out: DataOutput): Unit = {
      id          .write(out)
      Pat.serializer[Pat[A1]].write(outer, out)
      out.writeInt(token)
      mapStream   .write(out)
      _hasNext    .write(out)
      valid       .write(out)
      innerStream .write(out)
      itStream    .write(out)
    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      id          .dispose()
      mapStream   .dispose()
      _hasNext    .dispose()
      valid       .dispose()
      innerStream .dispose()
      mapItStreams.foreach(_.dispose())
    }

    final def mkItStream()(implicit ctx: Context[S], tx: S#Tx): ItStream[S, A1] = {
      val res = MapItStream.expand[S, A1](outer, token = token)
      mapItStreams.add(res)
      res
    }

    final def registerItStream(stream: ItStream[S, A1])(implicit tx: S#Tx): Unit =
      mapItStreams.add(stream)

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      logStream("PatMap.iterator.validate()")
      buildNext() // advance()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      mapItStreams /* ctx.getStreams(ref) */.foreach {
        case m: AdvanceItStream[S, _] => m.resetOuter()
      }
      innerStream.reset()
    }

    private def advance()(implicit ctx: Context[S], tx: S#Tx): Unit = {
      mapItStreams /* ctx.getStreams(ref) */.foreach {
        case m: AdvanceItStream[S, _] => m.advance()
      }
      innerStream.reset()
      buildNext()
    }

    private def buildNext()(implicit ctx: Context[S], tx: S#Tx): Unit = {
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
        val inner   = Pat(b.result: _*)
        mapStream() = inner
        _hasNext()  = true
      }
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
      ctx.withItSource(this) {
        hasNextI
      }

    private def hasNextI(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): Pat[A] =
      ctx.withItSource(this) {
        if (!hasNext) Stream.exhausted()
        val res = mapStream()
        advance()
        res
      }
  }
}

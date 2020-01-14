/*
 *  MapWithIndexImpl.scala
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

import de.sciss.lucre.stm.{Base, RefSet}
import de.sciss.patterns.graph.MapWithIndex
import de.sciss.serial.{DataInput, DataOutput}

object MapWithIndexImpl extends StreamFactory {
  final val typeId = 0x4D617049 // "MapI"

  def expand[S <: Base[S], A1, A](pat: MapWithIndex[A1, A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] = {
    import pat._
    val id            = tx.newId()
    val mapStream     = tx.newVar[Pat[A]](id, null)
    val _hasNext      = tx.newBooleanVar(id, false)
    val valid         = tx.newBooleanVar(id, false)

    new StreamNew[S, A1, A](ctx, tx, id = id, outer = outer,
      inTokenId = itIn.token, idxTokenId = itIdx.token,
      mapStream = mapStream, _hasNext = _hasNext, valid = valid, inner = inner)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id            = tx.readId(in, access)
    val outer         = Pat.serializer[Pat[Any]].read(in)
    val inTokenId     = in.readInt()
    val idxTokenId    = in.readInt()
    val mapStream     = tx.readVar[Pat[Any]](id, in)
    val _hasNext      = tx.readBooleanVar(id, in)
    val valid         = tx.readBooleanVar(id, in)

    new StreamRead[S, Any, Any](ctx, tx, in, access, id = id, outer = outer,
      inTokenId = inTokenId, idxTokenId = idxTokenId,
      mapStream = mapStream, _hasNext = _hasNext, valid = valid)
  }

  private final class StreamNew [S <: Base[S], A1, A](ctx0: Context[S], tx0: S#Tx,
                                                      id          : S#Id,
                                                      outer       : Pat[Pat[A1]],
                                                      inTokenId   : Int,
                                                      idxTokenId  : Int,
                                                      mapStream   : S#Var[Pat[A]],
                                                      _hasNext    : S#Var[Boolean],
                                                      valid       : S#Var[Boolean],
                                                      inner       : Pat[A]
                                                     )
    extends StreamImpl[S, A1, A](tx0, id, outer = outer, inTokenId = inTokenId, idxTokenId = idxTokenId,
      mapStream = mapStream, _hasNext = _hasNext, valid = valid) {

    protected val innerStream : Stream[S, A]  =
      ctx0.withItSources(ItInSource, ItIdxSource)(inner.expand[S](ctx0, tx0))(tx0)

    protected val itInStream  : Stream[S, A1] = ItInSource.mkItStream()(ctx0, tx0)
  }

  private final class StreamRead[S <: Base[S], A1, A](ctx0: Context[S], tx0: S#Tx, in0: DataInput, access0: S#Acc,
                                                      id          : S#Id,
                                                      outer       : Pat[Pat[A1]],
                                                      inTokenId   : Int,
                                                      idxTokenId  : Int,
                                                      mapStream   : S#Var[Pat[A]],
                                                      _hasNext    : S#Var[Boolean],
                                                      valid       : S#Var[Boolean]
                                                     )
    extends StreamImpl[S, A1, A](tx0, id, outer = outer, inTokenId = inTokenId, idxTokenId = idxTokenId,
      mapStream = mapStream, _hasNext = _hasNext, valid = valid) {

    protected val (innerStream: Stream[S, A], itInStream: Stream[S, A1]) = {
      ctx0.withItSources(ItInSource, ItIdxSource) {
        val _1 = Stream.read[S, A ](in0, access0)(ctx0, tx0)
        val _2 = Stream.read[S, A1](in0, access0)(ctx0, tx0)
        (_1, _2)
      } (tx0)
    }
  }

  private abstract class StreamImpl[S <: Base[S], A1, A](tx0: S#Tx,
                                                         id               : S#Id,
                                                         outer            : Pat[Pat[A1]],
                                                         inTokenId        : Int,
                                                         idxTokenId       : Int,
                                                         mapStream        : S#Var[Pat[A]],
                                                         _hasNext         : S#Var[Boolean],
                                                         valid            : S#Var[Boolean]
                                                        )
    extends Stream[S, Pat[A]] {

    // ---- abstract ----

    protected val innerStream : Stream[S, A]
    // because `inner` is not guaranteed to depend on `It`, we must
    // pro-active create one instance of the it-stream which is used
    // as an additional constraint to determine `hasNext`!
    protected val itInStream  : Stream[S, A1]

    // ---- impl ----

    private[this] val itStreams: RefSet[S, Stream[S, Any]] = tx0.newInMemorySet

    final protected object ItInSource extends ItStreamSource[S, A1] {
      def token: Int = inTokenId

      def mkItStream()(implicit ctx: Context[S], tx: S#Tx): ItStream[S, A1] = {
        val res = MapItStream.expand[S, A1](outer, token = token)
        itStreams.add(res)
        res
      }

      def registerItStream(stream: ItStream[S, A1])(implicit tx: S#Tx): Unit =
        itStreams.add(stream)
    }

    final protected object ItIdxSource extends ItStreamSource[S, Int] {
      def token: Int = idxTokenId

      def mkItStream()(implicit ctx: Context[S], tx: S#Tx): ItStream[S, Int] = {
        val res = IndexItStream.expand[S](token = token)
        itStreams.add(res)
        res
      }

      def registerItStream(stream: ItStream[S, Int])(implicit tx: S#Tx): Unit =
        itStreams.add(stream)
    }

    final protected def typeId: Int = MapWithIndexImpl.typeId

    final protected def writeData(out: DataOutput): Unit = {
      id          .write(out)
      Pat.serializer[Pat[A1]].write(outer, out)
      out.writeInt(inTokenId)
      out.writeInt(idxTokenId)
      mapStream   .write(out)
      _hasNext    .write(out)
      valid       .write(out)
      innerStream .write(out)
      itInStream  .write(out)
    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      id          .dispose()
      mapStream   .dispose()
      _hasNext    .dispose()
      valid       .dispose()
      innerStream .dispose()
      itStreams .foreach(_.dispose())
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      // $COVERAGE-OFF$
      logStream("MapWithIndex.iterator.validate()")
      // $COVERAGE-ON$
      buildNext()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      itStreams /* ctx.getStreams(refIn) */.foreach {
        case m: AdvanceItStream[S, _] => m.resetOuter()
      }
      innerStream.reset()
    }

    private def advance()(implicit ctx: Context[S], tx: S#Tx): Unit = {
      itStreams /* ctx.getStreams(refIn) */.foreach {
        case m: AdvanceItStream[S, _] => m.advance()
      }
      innerStream.reset()
      buildNext()
    }

    private def buildNext()(implicit ctx: Context[S], tx: S#Tx): Unit = {
      val hn = itInStream.hasNext // && innerStream.hasNext
      _hasNext() = hn
      if (hn) {
        //        val itIdxStreams = ctx.getStreams(ref)
//        itIdxStreams.foreach {
//          _.reset()
//        }
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
      ctx.withItSources(ItInSource, ItIdxSource) {
        hasNextI
      }

    private def hasNextI(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): Pat[A] =
      ctx.withItSources(ItInSource, ItIdxSource) {
        if (!hasNextI) Stream.exhausted()
        val res = mapStream()
        advance()
        res
      }
  }
}

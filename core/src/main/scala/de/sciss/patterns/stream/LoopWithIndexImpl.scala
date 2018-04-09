/*
 *  LoopWithIndexImpl.scala
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
import de.sciss.patterns.graph.LoopWithIndex
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.tailrec

object LoopWithIndexImpl extends StreamFactory {
  final val typeId = 0x4C704964 // "LpIdx"

  def expand[S <: Base[S], A](pat: LoopWithIndex[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val itStreams   = ctx.mkInMemorySet[Stream[S, Int]]
    val id          = tx.newId()
    val nStream     = n.expand[S]
    val nValue      = tx.newIntVar(id, 0)
    val iteration   = tx.newIntVar(id, 0)
    val _hasNext    = tx.newBooleanVar(id, false)
    val valid       = tx.newBooleanVar(id, false)

    new StreamNew[S, A](ctx, tx, id = id, nStream = nStream, tokenId = it.token, inner = inner,
      nValue = nValue, iteration = iteration, _hasNext = _hasNext, valid = valid, itStreams = itStreams)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val itStreams   = ctx.mkInMemorySet[Stream[S, Int]]
    val id          = tx.readId(in, access)
    val nStream     = Stream.read[S, Int](in, access)
    val tokenId     = in.readInt()
    val innerStream = Stream.read[S, Any](in, access)
    val nValue      = tx.readIntVar(id, in)
    val iteration   = tx.readIntVar(id, in)
    val _hasNext    = tx.readBooleanVar(id, in)
    val valid       = tx.readBooleanVar(id, in)

    new StreamRead[S, Any](id = id, nStream = nStream, tokenId = tokenId, innerStream = innerStream,
      nValue = nValue, iteration = iteration, _hasNext = _hasNext, valid = valid, itStreams = itStreams)
  }

  private final class StreamNew[S <: Base[S], A](ctx0: Context[S], tx0: S#Tx,
                                                   id         : S#Id,
                                                   nStream    : Stream[S, Int],
                                                   tokenId    : Int,
                                                   inner      : Pat[A],
                                                   nValue     : S#Var[Int],
                                                   iteration  : S#Var[Int],
                                                   _hasNext   : S#Var[Boolean],
                                                   valid      : S#Var[Boolean],
                                                   itStreams  : RefSet[S, Stream[S, Int]]
                                                 )
    extends StreamImpl[S, A](id = id, nStream = nStream, tokenId = tokenId,
      nValue = nValue, iteration = iteration, _hasNext = _hasNext, valid = valid, itStreams = itStreams) {

    protected val innerStream: Stream[S, A] = ctx0.withItSource(tokenId, this)(inner.expand[S](ctx0, tx0))(tx0)
  }

  private final class StreamRead[S <: Base[S], A](id         : S#Id,
                                                  nStream    : Stream[S, Int],
                                                  tokenId    : Int,
                                                  protected val innerStream: Stream[S, A],
                                                  nValue     : S#Var[Int],
                                                  iteration  : S#Var[Int],
                                                  _hasNext   : S#Var[Boolean],
                                                  valid      : S#Var[Boolean],
                                                  itStreams  : RefSet[S, Stream[S, Int]]
                                                 )
    extends StreamImpl[S, A](id = id, nStream = nStream, tokenId = tokenId,
      nValue = nValue, iteration = iteration, _hasNext = _hasNext, valid = valid, itStreams = itStreams)

  private abstract class StreamImpl[S <: Base[S], A](tokenId    : Int,
                                                     id         : S#Id,
                                                     nStream    : Stream[S, Int],
                                                     nValue     : S#Var[Int],
                                                     iteration  : S#Var[Int],
                                                     _hasNext   : S#Var[Boolean],
                                                     valid      : S#Var[Boolean],
                                                     itStreams  : RefSet[S, Stream[S, Int]]
                                                    )
    extends Stream[S, A] with ItStreamSource[S, Int] {

    // ---- abstract ----

    protected val innerStream: Stream[S, A]

    // ---- impl ----

    final protected def typeId: Int = LoopWithIndexImpl.typeId

    final protected def writeData(out: DataOutput): Unit = {
      id         .write(out)
      nStream    .write(out)
      out.writeInt(tokenId)
      innerStream.write(out)
      nValue     .write(out)
      iteration  .write(out)
      _hasNext   .write(out)
      valid      .write(out)
    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      id         .dispose()
      nStream    .dispose()
      innerStream.dispose()
      nValue     .dispose()
      iteration  .dispose()
      _hasNext   .dispose()
      valid      .dispose()
    }

    final def mkItStream()(implicit ctx: Context[S], tx: S#Tx): Stream[S, Int] = {
      val res = new IndexItStream[S](iteration, tx)
      itStreams.add(res)
      res
    }

    final def pingFromIt(stream: Stream[S, Int])(implicit tx: S#Tx): Unit =
      itStreams.add(stream)

    final def reset()(implicit tx: S#Tx): Unit = if (valid()) {
      valid() = false
      nStream.reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit =
      if (!valid()) {
        valid()     = true
        val nhn     = nStream.hasNext
        _hasNext()  = nhn
        if (nhn) {
          nValue()    = math.max(0, nStream.next())
          iteration() = 0
          nextIteration()
        }
      }

    final def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    @tailrec
    private def nextIteration()(implicit ctx: Context[S], tx: S#Tx): Unit = {
      val i   = iteration()
      val nhn = i < nValue()
      _hasNext()  = nhn
      if (nhn) {
        // val itStreams = ctx.getStreams(ref)
        itStreams.foreach(_.reset())
        innerStream.reset()
        val ihn = innerStream.hasNext
        _hasNext() = ihn
        if (!ihn) {
          iteration() = i + 1
          nextIteration()
        }
      }
    }

    final def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = innerStream.next()
      val ihn = innerStream.hasNext
      _hasNext() = ihn
      if (!ihn) {
        val i = iteration()
        if (i < nValue()) {
          iteration() = i + 1
          nextIteration()
        }
      }
      res
    }
  }
}

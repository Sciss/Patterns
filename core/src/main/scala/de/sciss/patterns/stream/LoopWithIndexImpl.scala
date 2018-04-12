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
    val id          = tx.newId()
    val nStream     = n.expand[S]
    val nValue      = tx.newIntVar(id, 0)
    val iteration   = tx.newIntVar(id, 0)
    val _hasNext    = tx.newBooleanVar(id, false)
    val valid       = tx.newBooleanVar(id, false)

    new StreamNew[S, A](ctx, tx, id = id, nStream = nStream, tokenId = it.token,
      nValue = nValue, iteration = iteration, _hasNext = _hasNext, valid = valid, inner = inner)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id          = tx.readId(in, access)
    val nStream     = Stream.read[S, Int](in, access)
    val tokenId     = in.readInt()
    val nValue      = tx.readIntVar(id, in)
    val iteration   = tx.readIntVar(id, in)
    val _hasNext    = tx.readBooleanVar(id, in)
    val valid       = tx.readBooleanVar(id, in)

    new StreamRead[S, Any](ctx, tx, in, access, id = id, nStream = nStream, tokenId = tokenId,
      nValue = nValue, iteration = iteration, _hasNext = _hasNext, valid = valid)
  }

  private final class StreamNew[S <: Base[S], A](ctx0: Context[S], tx0: S#Tx,
                                                   id         : S#Id,
                                                   nStream    : Stream[S, Int],
                                                   tokenId    : Int,
                                                   nValue     : S#Var[Int],
                                                   iteration  : S#Var[Int],
                                                   _hasNext   : S#Var[Boolean],
                                                   valid      : S#Var[Boolean],
                                                   inner      : Pat[A]
                                                 )
    extends StreamImpl[S, A](tx0, id = id, nStream = nStream, token = tokenId,
      nValue = nValue, iteration = iteration, _hasNext = _hasNext, valid = valid) {

    protected val innerStream: Stream[S, A] =
      ctx0.withItSource(this)(inner.expand[S](ctx0, tx0))(tx0)
  }

  private final class StreamRead[S <: Base[S], A](ctx0: Context[S], tx0: S#Tx, in0: DataInput, access0: S#Acc,
                                                  id         : S#Id,
                                                  nStream    : Stream[S, Int],
                                                  tokenId    : Int,
                                                  nValue     : S#Var[Int],
                                                  iteration  : S#Var[Int],
                                                  _hasNext   : S#Var[Boolean],
                                                  valid      : S#Var[Boolean],
                                                 )
    extends StreamImpl[S, A](tx0, id = id, nStream = nStream, token = tokenId,
      nValue = nValue, iteration = iteration, _hasNext = _hasNext, valid = valid) {

    protected val innerStream: Stream[S, A] =
      ctx0.withItSource(this)(Stream.read[S, A](in0, access0)(ctx0, tx0))(tx0)
  }

  private abstract class StreamImpl[S <: Base[S], A](tx0: S#Tx,
                                                     final val token: Int,
                                                     id               : S#Id,
                                                     nStream          : Stream[S, Int],
                                                     nValue           : S#Var[Int],
                                                     iteration        : S#Var[Int],
                                                     _hasNext         : S#Var[Boolean],
                                                     valid            : S#Var[Boolean],
                                                    )
    extends Stream[S, A] with ItStreamSource[S, Int] {

    // ---- abstract ----

    protected val innerStream: Stream[S, A]

    // ---- impl ----

    final protected val itStreams = tx0.newInMemorySet[ItStream[S, Int]]

    final protected def typeId: Int = LoopWithIndexImpl.typeId

    final protected def writeData(out: DataOutput): Unit = {
      id         .write(out)
      nStream    .write(out)
      out.writeInt(token)
      nValue     .write(out)
      iteration  .write(out)
      _hasNext   .write(out)
      valid      .write(out)
      innerStream.write(out)
    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      id         .dispose()
      nStream    .dispose()
      nValue     .dispose()
      iteration  .dispose()
      _hasNext   .dispose()
      valid      .dispose()
      innerStream.dispose()
    }

    final def mkItStream()(implicit ctx: Context[S], tx: S#Tx): ItStream[S, Int] = {
      val res = IndexItStream.expand[S](token)
      itStreams.add(res)
      res
    }

    final def registerItStream(stream: ItStream[S, Int])(implicit tx: S#Tx): Unit =
      itStreams.add(stream)

    final def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      itStreams /* ctx.getStreams(refIn) */.foreach {
        case m: AdvanceItStream[S, _] => m.resetOuter()
      }
      nStream.reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      val nhn     = nStream.hasNext
      _hasNext()  = nhn
      if (nhn) {
        nValue() = math.max(0, nStream.next())
        buildNext(adv = false)
      }
    }

    final def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
      ctx.withItSource(this) {
        hasNextI
      }

    private def hasNextI(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    private def advance()(implicit ctx: Context[S], tx: S#Tx): Unit =
      buildNext(adv = true)

    @tailrec
    private def buildNext(adv: Boolean)(implicit ctx: Context[S], tx: S#Tx): Unit = {
      val i = if (adv) {
        itStreams /* ctx.getStreams(refIn) */.foreach {
          case m: AdvanceItStream[S, _] => m.advance()
        }
        iteration() + 1
      } else {
        0
      }
      iteration() = i
      val nhn = i < nValue()
      _hasNext()  = nhn
      if (nhn) {
        // val itStreams = ctx.getStreams(ref)
//        itStreams.foreach(_.reset())
        innerStream.reset()
        val ihn = innerStream.hasNext
        _hasNext() = ihn
        if (!ihn) {
          buildNext(adv = adv)
        }
      }
    }

    final def next()(implicit ctx: Context[S], tx: S#Tx): A =
      ctx.withItSource(this) {
        if (!hasNextI) Stream.exhausted()
        val res = innerStream.next()
        val ihn = innerStream.hasNext
        _hasNext() = ihn
        if (!ihn) {
          advance()
        }
        res
      }
  }
}

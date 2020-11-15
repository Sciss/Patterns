/*
 *  LoopWithIndexImpl.scala
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
import de.sciss.patterns.graph.{LoopWithIndex, Pat}
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.tailrec

object LoopWithIndexImpl extends StreamFactory {
  final val typeId = 0x4C704964 // "LpIdx"

  def expand[T <: Exec[T], A](pat: LoopWithIndex[A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val id          = tx.newId()
    val nStream     = n.expand[T]
    val nValue      = id.newIntVar(0)
    val iteration   = id.newIntVar(0)
    val _hasNext    = id.newBooleanVar(false)
    val valid       = id.newBooleanVar(false)

    new StreamNew[T, A](ctx, tx, id = id, nStream = nStream, token = it.token,
      nValue = nValue, iteration = iteration, _hasNext = _hasNext, valid = valid, inner = inner)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id          = tx.readId(in)
    val nStream     = Stream.read[T, Int](in)
    val tokenId     = in.readInt()
    val nValue      = id.readIntVar(in)
    val iteration   = id.readIntVar(in)
    val _hasNext    = id.readBooleanVar(in)
    val valid       = id.readBooleanVar(in)

    new StreamRead[T, Any](ctx, tx, in, id = id, nStream = nStream, token = tokenId,
      nValue = nValue, iteration = iteration, _hasNext = _hasNext, valid = valid)
  }

  private final class StreamCopy[T <: Exec[T], A](tx0: T,
                                                  id         : Ident[T],
                                                  nStream    : Stream[T, Int],
                                                  token      : Int,
                                                  nValue     : Var[T, Int],
                                                  iteration  : Var[T, Int],
                                                  _hasNext   : Var[T, Boolean],
                                                  valid      : Var[T, Boolean],
                                                  protected val innerStream: Stream[T, A],
                                                 )
    extends StreamImpl[T, A](tx0, id = id, nStream = nStream, token = token,
      nValue = nValue, iteration = iteration, _hasNext = _hasNext, valid = valid)

  private final class StreamNew[T <: Exec[T], A](ctx0: Context[T], tx0: T,
                                                 id         : Ident[T],
                                                 nStream    : Stream[T, Int],
                                                 token      : Int,
                                                 nValue     : Var[T, Int],
                                                 iteration  : Var[T, Int],
                                                 _hasNext   : Var[T, Boolean],
                                                 valid      : Var[T, Boolean],
                                                 inner      : Pat[A]
                                                 )
    extends StreamImpl[T, A](tx0, id = id, nStream = nStream, token = token,
      nValue = nValue, iteration = iteration, _hasNext = _hasNext, valid = valid) {

    protected val innerStream: Stream[T, A] =
      ctx0.withItSource(this)(inner.expand[T](ctx0, tx0))(tx0)
  }

  private final class StreamRead[T <: Exec[T], A](ctx0: Context[T], tx0: T, in0: DataInput,
                                                  id         : Ident[T],
                                                  nStream    : Stream[T, Int],
                                                  token      : Int,
                                                  nValue     : Var[T, Int],
                                                  iteration  : Var[T, Int],
                                                  _hasNext   : Var[T, Boolean],
                                                  valid      : Var[T, Boolean]
                                                 )
    extends StreamImpl[T, A](tx0, id = id, nStream = nStream, token = token,
      nValue = nValue, iteration = iteration, _hasNext = _hasNext, valid = valid) {

    protected val innerStream: Stream[T, A] =
      ctx0.withItSource(this)(Stream.read[T, A](in0)(ctx0, tx0))(tx0)
  }

  private abstract class StreamImpl[T <: Exec[T], A](tx0: T,
                                                     final val token: Int,
                                                     id               : Ident[T],
                                                     nStream          : Stream[T, Int],
                                                     nValue           : Var[T, Int],
                                                     iteration        : Var[T, Int],
                                                     _hasNext         : Var[T, Boolean],
                                                     valid            : Var[T, Boolean]
                                                    )
    extends Stream[T, A] with ItStreamSource[T, Int] {

    // ---- abstract ----

    protected val innerStream: Stream[T, A]

    // ---- impl ----

    private[patterns] final def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                            (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut           = txOut.newId()
      val nStreamOut      = c(nStream)
      val nValueOut       = idOut.newIntVar(nValue())
      val iterationOut    = idOut.newIntVar(iteration())
      val hasNextOut      = idOut.newBooleanVar(_hasNext())
      val validOut        = idOut.newBooleanVar(valid())
      val innerStreamOut  = c(innerStream)

      new StreamCopy[Out, A](txOut, id = idOut, nStream = nStreamOut, token = token,
        nValue = nValueOut, iteration = iterationOut, _hasNext = hasNextOut,
        valid = validOut, innerStream = innerStreamOut)
    }

    final protected val itStreams = tx0.newInMemorySet[ItStream[T, Int]]

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

    final def dispose()(implicit tx: T): Unit = {
      id         .dispose()
      nStream    .dispose()
      nValue     .dispose()
      iteration  .dispose()
      _hasNext   .dispose()
      valid      .dispose()
      innerStream.dispose()
    }

    final def mkItStream()(implicit ctx: Context[T], tx: T): ItStream[T, Int] = {
      val res = IndexItStream.expand[T](token)
      itStreams.add(res)
      res
    }

    final def registerItStream(stream: ItStream[T, Int])(implicit tx: T): Unit =
      itStreams.add(stream)

    final def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      itStreams /* ctx.getStreams(refIn) */.foreach {
        case m: AdvanceItStream[T, _] => m.resetOuter()
      }
      nStream.reset()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      val nhn     = nStream.hasNext
      _hasNext()  = nhn
      if (nhn) {
        nValue() = math.max(0, nStream.next())
        buildNext(adv = false)
      }
    }

    final def hasNext(implicit ctx: Context[T], tx: T): Boolean =
      ctx.withItSource(this) {
        hasNextI
      }

    private def hasNextI(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    private def advance()(implicit ctx: Context[T], tx: T): Unit =
      buildNext(adv = true)

    @tailrec
    private def buildNext(adv: Boolean)(implicit ctx: Context[T], tx: T): Unit = {
      val i = if (adv) {
        itStreams /* ctx.getStreams(refIn) */.foreach {
          case m: AdvanceItStream[T, _] => m.advance()
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
          buildNext(adv = true)
        }
      }
    }

    final def next()(implicit ctx: Context[T], tx: T): A =
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

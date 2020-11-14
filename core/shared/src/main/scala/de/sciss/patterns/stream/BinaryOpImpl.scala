/*
 *  BinaryOpImpl.scala
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

import de.sciss.lucre.Adjunct
import de.sciss.lucre.Adjunct.Widen2
import de.sciss.lucre.Exec
import de.sciss.patterns.graph.BinaryOp
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

object BinaryOpImpl extends StreamFactory {
  final val typeId = 0x42696E61 // "Bina"

  def expand[T <: Exec[T], A1, A2, A3, A](pat: BinaryOp[A1, A2, A3, A])
                                         (implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val aStream     = a.expand[T]
    val bStream     = b.expand[T]
    val state       = op.prepare(ref)

    new StreamImpl[T, A1, A2, A3, A, op.State](op = op, state = state, aStream = aStream, bStream = bStream)(widen)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val op          = PatElem.read[BinaryOp.Op[Any, Any]](in)
    val state       = op.readState(in)
    val aStream     = Stream.read[T, Pat[Any]](in)
    val bStream     = Stream.read[T, Pat[Any]](in)
    val widen       = Adjunct.readT[Widen2[Any, Any, Any]](in)

    new StreamImpl[T, Any, Any, Any, Any, op.State](op = op, state = state, aStream = aStream, bStream = bStream)(widen)
  }

  private final class StreamImpl[T <: Exec[T], A1, A2, A3, A, St[~ <: Exec[~]]](
                                                                                 op: BinaryOp.Op[A3, A] { type State[~ <: Exec[~]] = St[~] },
                                                                                 state: St[T],
                                                                                 aStream: Stream[T, A1],
                                                                                 bStream: Stream[T, A2]
  ) (
    implicit widen: Widen2[A1, A2, A3]
  )
    extends Stream[T, A] {

    private[patterns] override def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                               (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val aStreamOut     = c(aStream)
      val bStreamOut     = c(bStream)
      val stateOut       = op.copyState[T, Out](state)
      new StreamImpl[Out, A1, A2, A3, A, op.State](op = op, state = stateOut, aStream = aStreamOut,
        bStream = bStreamOut)(widen)
    }

    protected def typeId: Int = BinaryOpImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      PatElem.write(op, out)
      op.writeState[T](state, out)
      aStream .write(out)
      bStream .write(out)
      widen   .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      op.disposeState(state)
      aStream .dispose()
      bStream .dispose()
    }

    def reset()(implicit tx: T): Unit = {
      aStream.reset()
      bStream.reset()
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean =
      aStream.hasNext && bStream.hasNext

    def next()(implicit ctx: Context[T], tx: T): A = {
      val aVal = widen.widen1(aStream.next())
      val bVal = widen.widen2(bStream.next())
      op.next(aVal, bVal)(state, tx)
    }
  }
}
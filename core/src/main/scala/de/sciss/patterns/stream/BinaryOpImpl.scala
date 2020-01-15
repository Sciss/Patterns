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

import de.sciss.lucre.adjunct.Adjunct
import de.sciss.lucre.adjunct.Adjunct.Widen2
import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.BinaryOp
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

import scala.language.higherKinds

object BinaryOpImpl extends StreamFactory {
  final val typeId = 0x42696E61 // "Bina"

  def expand[S <: Base[S], A1, A2, A3, A](pat: BinaryOp[A1, A2, A3, A])
                                         (implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val aStream     = a.expand[S]
    val bStream     = b.expand[S]
    val state       = op.prepare(ref)

    new StreamImpl[S, A1, A2, A3, A, op.State](op = op, state = state, aStream = aStream, bStream = bStream)(widen)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val op          = PatElem.read[BinaryOp.Op[Any, Any]](in)
    val state       = op.readState(in, access)
    val aStream     = Stream.read[S, Pat[Any]](in, access)
    val bStream     = Stream.read[S, Pat[Any]](in, access)
    val widen       = Adjunct.readT[Widen2[Any, Any, Any]](in)

    new StreamImpl[S, Any, Any, Any, Any, op.State](op = op, state = state, aStream = aStream, bStream = bStream)(widen)
  }

  private final class StreamImpl[S <: Base[S], A1, A2, A3, A, St[~ <: Base[~]]](
                                                                                 op: BinaryOp.Op[A3, A] { type State[~ <: Base[~]] = St[~] },
                                                                                 state: St[S],
                                                                                 aStream: Stream[S, A1],
                                                                                 bStream: Stream[S, A2]
  ) (
    implicit widen: Widen2[A1, A2, A3]
  )
    extends Stream[S, A] {

    private[patterns] override def copyStream[Out <: Base[Out]]()(implicit tx: S#Tx, txOut: Out#Tx,
                                                                  ctx: Context[Out]): Stream[Out, A] = {
      val aStreamOut     = aStream.copyStream[Out]()
      val bStreamOut     = bStream.copyStream[Out]()
      val stateOut       = op.copyState[S, Out](state)
      new StreamImpl[Out, A1, A2, A3, A, op.State](op = op, state = stateOut, aStream = aStreamOut,
        bStream = bStreamOut)(widen)
    }

    protected def typeId: Int = BinaryOpImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      PatElem.write(op, out)
      op.writeState[S](state, out)
      aStream .write(out)
      bStream .write(out)
      widen   .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      op.disposeState(state)
      aStream .dispose()
      bStream .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = {
      aStream.reset()
      bStream.reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
      aStream.hasNext && bStream.hasNext

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      val aVal = widen.widen1(aStream.next())
      val bVal = widen.widen2(bStream.next())
      op.next(aVal, bVal)(state, tx)
    }
  }
}
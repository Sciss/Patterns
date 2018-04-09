/*
 *  BinaryOpImpl.scala
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
import de.sciss.patterns
import de.sciss.patterns.Types.{Aux, Widen2}
import de.sciss.patterns.graph.BinaryOp
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

import scala.language.higherKinds

object BinaryOpImpl extends StreamFactory {
  final val typeId = 0x42696E61 // "Bina"

  def expand[S <: Base[S], A1, A2, A3, A](pat: BinaryOp[A1, A2, A3, A])
                                         (implicit ctx: Context[S], tx: S#Tx): patterns.Stream[S, A] = {
    import pat._
    val aStream     = a.expand[S]
    val bStream     = b.expand[S]
    val state       = op.prepare(ref)

    new StreamImpl[S, A1, A2, A3, A, op.State](op = op, state = state, aStream = aStream, bStream = bStream)(widen)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): patterns.Stream[S, A] = {
    val op          = PatElem.read[BinaryOp.Op[Any, A]](in)
    val state       = op.readState(in, access)
    val aStream     = patterns.Stream.read[S, Pat[Any]](in, access)
    val bStream     = patterns.Stream.read[S, Pat[Any]](in, access)
    val widen       = Aux.readT[Widen2[Any, Any, Any]](in)

    new StreamImpl[S, Any, Any, Any, A, op.State](op = op, state = state, aStream = aStream, bStream = bStream)(widen)
  }

  private final class StreamImpl[S <: Base[S], A1, A2, A3, A, St[~ <: Base[~]]](
                                                                                 op: BinaryOp.Op[A3, A] { type State[~ <: Base[~]] = St[~] },
                                                                                 state: St[S],
                                                                                 aStream: patterns.Stream[S, A1],
                                                                                 bStream: patterns.Stream[S, A2]
  ) (
    implicit widen: Widen2[A1, A2, A3]
  )
    extends Stream[S, A] {

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
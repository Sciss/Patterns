/*
 *  UnaryOpImpl.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package stream

import de.sciss.lucre.Exec
import de.sciss.patterns.graph.UnaryOp
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

object UnaryOpImpl extends StreamFactory {
  final val typeId = 0x556E6172 // "Unar"

  def expand[T <: Exec[T], A1, A](pat: UnaryOp[A1, A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    // careful - very fragile (Scala 2 vs 3)
    import pat._
    val aStream     = a.expand[T]
    val state       = op.prepare[T](ref)
    new StreamImpl[T, A1, A, op.State](op = op , state = state, aStream = aStream)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] =
    readIdentifiedT[T, Any, Any](in)

  private def readIdentifiedT[T <: Exec[T], A1, A](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, A] = {
    // careful - very fragile (Scala 2 vs 3)
    val op          = PatElem.read[UnaryOp.Op[A1, A]](in)
    val state       = op.readState[T](in)
    val aStream     = Stream.read[T, A1](in)
    val st: Stream[T, A] = new StreamImpl[T, A1, A, op.State](op = op, state = state, aStream = aStream)
    st
  }

  private final class StreamImpl[T <: Exec[T], A1, A, St[~ <: Exec[~]]](
    op: UnaryOp.Op[A1, A] { type State[~ <: Exec[~]] = St[~] },
    state: St[T],
    aStream: Stream[T, A1]
  )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      // careful - very fragile (Scala 2 vs 3)
      val aStreamOut    = c(aStream)
      val stateOut      = op.copyState[T, Out](state)
      val st: Stream[Out, A] = new StreamImpl[Out, A1, A, op.State](op = op, state = stateOut, aStream = aStreamOut)
      st
    }

    protected def typeId: Int = UnaryOpImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      PatElem.write(op, out)
      op.writeState[T](state, out)
      aStream .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      op.disposeState(state)
      aStream .dispose()
    }

    def reset()(implicit tx: T): Unit =
      aStream.reset()

    def hasNext(implicit ctx: Context[T], tx: T): Boolean =
      aStream.hasNext

    def next()(implicit ctx: Context[T], tx: T): A = {
      val aVal = aStream.next()
      op.next(aVal)(state, tx)
    }
  }
}

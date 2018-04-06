package de.sciss.patterns
package graph
package impl

import de.sciss.lucre.stm.Base
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

import scala.language.higherKinds

object UnaryOpImpl extends StreamFactory {
  final val typeId = 0x556E6172 // "Unar"

  def expand[S <: Base[S], A1, A](pat: UnaryOp[A1, A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val aStream     = a.expand[S]
    val state       = op.prepare(ref)

    new StreamImpl[S, A1, A, op.State](op = op, state = state, aStream = aStream)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    val op          = PatElem.read[UnaryOp.Op[Any, A]](in)
    val state       = op.readState(in, access)
    val aStream     = Stream.read[S, Pat[Any]](in, access)

    new StreamImpl[S, Any, A, op.State](op = op, state = state, aStream = aStream)
  }

  private final class StreamImpl[S <: Base[S], A1, A, St[~ <: Base[~]]](
    op: UnaryOp.Op[A1, A] { type State[~ <: Base[~]] = St[~] },
    state: St[S],
    aStream: Stream[S, A1]
  )
    extends Stream[S, A] {

    protected def typeId: Int = UnaryOpImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      PatElem.write(op, out)
      op.writeState[S](state, out)
      aStream .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      op.disposeState(state)
      aStream .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit =
      aStream.reset()

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
      aStream.hasNext

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      val aVal = aStream.next()
      op.next(aVal)(state, tx)
    }
  }
}

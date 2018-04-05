package de.sciss.patterns
package graph
package impl

import de.sciss.lucre.stm.{Base, TxnRandom}
import de.sciss.patterns.Types.Num
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.DataInput

object WhiteImpl {
  def newStream[S <: Base[S], A](pat: White[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id        = tx.newId()
    val loStream  = lo.expand[S]
    val hiStream  = hi.expand[S]
    val state     = PatElem.makeVar[S, A](id)
    val _hasNext  = tx.newBooleanVar(id, false)
    val valid     = tx.newBooleanVar(id, false)
    val r         = ctx.mkRandom(ref)

    new StreamImpl[S, A](id = id, loStream = loStream, hiStream = hiStream, state = state, _hasNext = _hasNext,
      valid = valid)(r, num)
  }

  def readStream[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    ???
  }

  private final class StreamImpl[S <: Base[S], A](
    id: S#Id, loStream: Stream[S, A], hiStream: Stream[S, A],
    state: S#Var[A], _hasNext: S#Var[Boolean],
    valid: S#Var[Boolean]
  )(
    implicit r: TxnRandom[S], num: Num[A]
  )
    extends Stream[S, A] {

    private def mkState()(implicit tx: S#Tx): A = num.rrand(loStream.next(), hiStream.next())

    def reset()(implicit tx: S#Tx): Unit = if (valid()) {
      valid() = false
      loStream.reset()
      hiStream.reset()
      // XXX TODO: r.reset()
    }

    def hasNext(implicit tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    private def validate()(implicit tx: S#Tx): Unit =
      if (!valid()) {
        valid() = true
        _hasNext() = loStream.hasNext && hiStream.hasNext
        if (_hasNext()) {
          state() = num.rrand(loStream.next(), hiStream.next())
        }
      }

    def next()(implicit tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = state()
      _hasNext() = loStream.hasNext && hiStream.hasNext
      if (_hasNext()) {
        state() = mkState()
      }
      res
    }
  }

}

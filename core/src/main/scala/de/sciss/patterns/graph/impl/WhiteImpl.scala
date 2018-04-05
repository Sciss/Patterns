/*
 *  WhiteImpl.scala
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
package graph
package impl

import de.sciss.lucre.stm.{Base, TxnRandom}
import de.sciss.patterns.Types.{Aux, Num}
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.DataInput

object WhiteImpl {
  def expand[S <: Base[S], A](pat: White[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
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

  def read[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    val id        = tx.readId(in, access)
    val num       = Aux.readT[Num[A]](in)

    val loStream  = Stream.read[S, A](in, access)
    val hiStream  = Stream.read[S, A](in, access)
    val state     = PatElem.makeVar[S, A](id)
    val _hasNext  = tx.readBooleanVar(id, in)
    val valid     = tx.readBooleanVar(id, in)
    val r         = TxnRandom.read(in, access)

    new StreamImpl[S, A](id = id, loStream = loStream, hiStream = hiStream, state = state, _hasNext = _hasNext,
      valid = valid)(r, num)
  }

  private final class StreamImpl[S <: Base[S], A](
    id: S#Id, loStream: Stream[S, A], hiStream: Stream[S, A],
    state: S#Var[A], _hasNext: S#Var[Boolean],
    valid: S#Var[Boolean]
  )(
    implicit r: TxnRandom[S], num: Num[A]
  )
    extends Stream[S, A] {

    private def mkState()(implicit ctx: Context[S], tx: S#Tx): A =
      num.rrand(loStream.next(), hiStream.next())

    def reset()(implicit tx: S#Tx): Unit = if (valid()) {
      valid() = false
      loStream.reset()
      hiStream.reset()
      // XXX TODO: r.reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit =
      if (!valid()) {
        valid() = true
        _hasNext() = loStream.hasNext && hiStream.hasNext
        if (_hasNext()) {
          state() = num.rrand(loStream.next(), hiStream.next())
        }
      }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
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

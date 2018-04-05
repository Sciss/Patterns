/*
 *  BrownImpl.scala
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

import de.sciss.lucre.stm.{Base, Random, TxnRandom}
import de.sciss.patterns.Types.{Aux, Num, Widen2}
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.DataInput

object BrownImpl {
  def expand[S <: Base[S], A1, A2, A](pat: Brown[A1, A2, A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id          = tx.newId()

    val loStream    = lo.expand[S]
    val hiStream    = hi.expand[S]
    val stepStream  = step.expand[S]
    val state       = PatElem.makeVar[S, A](id)
    val _hasNext    = tx.newBooleanVar(id, false)
    val valid       = tx.newBooleanVar(id, false)
    val r           = ctx.mkRandom(ref)

    new StreamImpl[S, A1, A2, A](id = id, loStream = loStream, hiStream = hiStream, stepStream = stepStream,
      state = state, _hasNext = _hasNext,
      valid = valid)(r, widen, num)
  }

  def read[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    val id          = tx.readId(in, access)
    val widen       = Aux.readT[Widen2[Any, Any, A]](in)
    val num         = Aux.readT[Num[A]](in)

    val loStream    = Stream.read[S, Any](in, access)
    val hiStream    = Stream.read[S, Any](in, access)
    val stepStream  = Stream.read[S, A](in, access)
    val state       = PatElem.readVar[S, A](id, in)
    val _hasNext    = tx.newBooleanVar(id, false)
    val valid       = tx.newBooleanVar(id, false)
    val r           = TxnRandom.read[S](in, access)

    new StreamImpl[S, Any, Any, A](id = id, loStream = loStream, hiStream = hiStream, stepStream = stepStream,
      state = state, _hasNext = _hasNext,
      valid = valid)(r, widen, num)
  }

  private final class StreamImpl[S <: Base[S], A1, A2, A](
    id        : S#Id,
    loStream  : Stream[S, A1],
    hiStream  : Stream[S, A1],
    stepStream: Stream[S, A2],
    state     : S#Var[A],
    _hasNext  : S#Var[Boolean],
    valid     : S#Var[Boolean]
  )(
    implicit r: TxnRandom[S],
    widen: Widen2[A1, A2, A],
    num: Num[A]
  )
    extends Stream[S, A] {

    import widen._

    @inline
    private def calcNext(cur: A, step: A)(implicit r: Random[S#Tx], tx: S#Tx): A =
      num.+(cur, num.rand2(step))

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit =
      if (!valid()) {
        valid() = true
        _hasNext() = loStream.hasNext && hiStream.hasNext
        if (_hasNext()) {
          state() = num.rrand(widen1(loStream.next()), widen1(hiStream.next()))
        }
      }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid()) {
      valid() = false
      loStream  .reset()
      hiStream  .reset()
      stepStream.reset()
      // XXX TODO: r.reset()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = state()
      _hasNext() = loStream.hasNext && hiStream.hasNext && stepStream.hasNext
      if (_hasNext()) {
        val loVal   = loStream.next()
        val hiVal   = hiStream.next()
        val stepVal = stepStream.next()
        val x       = calcNext(res, widen2(stepVal))
        state()     = num.fold(x, widen1(loVal), widen1(hiVal))
      }
      res
    }
  }
}

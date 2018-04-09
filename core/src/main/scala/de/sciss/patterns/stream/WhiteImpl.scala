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
package stream

import de.sciss.lucre.stm.{Base, TxnRandom}
import de.sciss.patterns.Types.{Aux, Num}
import de.sciss.patterns.graph.White
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

object WhiteImpl extends StreamFactory {
  final val typeId = 0x57686974 // "Whit"

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

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    val id        = tx.readId(in, access)
    val loStream  = Stream.read[S, A](in, access)
    val hiStream  = Stream.read[S, A](in, access)
    val state     = PatElem.makeVar[S, A](id)
    val _hasNext  = tx.readBooleanVar(id, in)
    val valid     = tx.readBooleanVar(id, in)
    val r         = TxnRandom.read(in, access)
    val num       = Aux.readT[Num[A]](in)

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

    protected def typeId: Int = WhiteImpl.typeId

    def dispose()(implicit tx: S#Tx): Unit = {
      id      .dispose()
      loStream.dispose()
      hiStream.dispose()
      state   .dispose()
      _hasNext.dispose()
      valid   .dispose()
      r       .dispose()
    }

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      loStream.write(out)
      hiStream.write(out)
      state   .write(out)
      _hasNext.write(out)
      valid   .write(out)
      r       .write(out)
//      Aux.write(out, num)
      num     .write(out)
    }

    private def mkState()(implicit ctx: Context[S], tx: S#Tx): A =
      num.rrand(loStream.next(), hiStream.next())

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      loStream.reset()
      hiStream.reset()
      // XXX TODO: r.reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
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

/*
 *  WhiteImpl.scala
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
import de.sciss.lucre.adjunct.Adjunct.Num
import de.sciss.lucre.stm.{Base, TxnRandom}
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

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id        = tx.readId(in, access)
    val loStream  = Stream.read[S, Any](in, access)
    val hiStream  = Stream.read[S, Any](in, access)
    val state     = PatElem.readVar[S, Any](id, in)
    val _hasNext  = tx.readBooleanVar(id, in)
    val valid     = tx.readBooleanVar(id, in)
    val r         = TxnRandom.read(in, access)
    val num       = Adjunct.readT[Num[Any]](in)

    new StreamImpl[S, Any](id = id, loStream = loStream, hiStream = hiStream, state = state, _hasNext = _hasNext,
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

    private[patterns] def copyStream[Out <: Base[Out]]()(implicit tx: S#Tx, txOut: Out#Tx,
                                                         ctx: Context[Out]): Stream[Out, A] = {
      val idOut       = txOut.newId()
      val loStreamOut = loStream.copyStream[Out]()
      val hiStreamOut = hiStream.copyStream[Out]()
      val stateOut    = PatElem.copyVar[Out, A](idOut, state())
      val hasNextOut  = txOut.newBooleanVar(idOut, _hasNext())
      val validOut    = txOut.newBooleanVar(idOut, valid())
      val rOut        = r.copy[Out]()

      new StreamImpl[Out, A](id = idOut, loStream = loStreamOut, hiStream = hiStreamOut, state = stateOut, _hasNext = hasNextOut,
        valid = validOut)(rOut, num)
    }

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
//      Adjunct.write(out, num)
      num     .write(out)
    }

    private def mkState()(implicit ctx: Context[S], tx: S#Tx): A =
      num.rangeRand(loStream.next(), hiStream.next())

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
        state() = num.rangeRand(loStream.next(), hiStream.next())
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

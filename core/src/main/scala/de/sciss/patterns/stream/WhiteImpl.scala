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

import de.sciss.lucre.{Adjunct, Exec, Ident, RandomObj, Var}
import de.sciss.lucre.Adjunct.Num
import de.sciss.patterns.graph.White
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

object WhiteImpl extends StreamFactory {
  final val typeId = 0x57686974 // "Whit"

  def expand[T <: Exec[T], A](pat: White[A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val id        = tx.newId()
    val loStream  = lo.expand[T]
    val hiStream  = hi.expand[T]
    val state     = PatElem.makeVar[T, A](id)
    val _hasNext  = id.newBooleanVar(false)
    val valid     = id.newBooleanVar(false)
    val r         = ctx.mkRandom(ref)

    new StreamImpl[T, A](id = id, loStream = loStream, hiStream = hiStream, state = state, _hasNext = _hasNext,
      valid = valid)(r, num)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id        = tx.readId(in)
    val loStream  = Stream.read[T, Any](in)
    val hiStream  = Stream.read[T, Any](in)
    val state     = PatElem.readVar[T, Any](id, in)
    val _hasNext  = id.readBooleanVar(in)
    val valid     = id.readBooleanVar(in)
    val r         = RandomObj.read(in)
    val num       = Adjunct.readT[Num[Any]](in)

    new StreamImpl[T, Any](id = id, loStream = loStream, hiStream = hiStream, state = state, _hasNext = _hasNext,
      valid = valid)(r, num)
  }

  private final class StreamImpl[T <: Exec[T], A](
                                                   id: Ident[T], loStream: Stream[T, A], hiStream: Stream[T, A],
                                                   state: Var[T, A], _hasNext: Var[T, Boolean],
                                                   valid: Var[T, Boolean]
  )(
    implicit r: RandomObj[T], num: Num[A]
  )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut       = txOut.newId()
      val loStreamOut = c(loStream)
      val hiStreamOut = c(hiStream)
      val stateOut    = PatElem.copyVar[Out, A](idOut, state())
      val hasNextOut  = idOut.newBooleanVar(_hasNext())
      val validOut    = idOut.newBooleanVar(valid())
      val rOut        = r.copy[Out]()

      new StreamImpl[Out, A](id = idOut, loStream = loStreamOut, hiStream = hiStreamOut, state = stateOut, _hasNext = hasNextOut,
        valid = validOut)(rOut, num)
    }

    protected def typeId: Int = WhiteImpl.typeId

    def dispose()(implicit tx: T): Unit = {
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

    private def mkState()(implicit ctx: Context[T], tx: T): A =
      num.rangeRand(loStream.next(), hiStream.next())

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      loStream.reset()
      hiStream.reset()
      // XXX TODO: r.reset()
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      _hasNext() = loStream.hasNext && hiStream.hasNext
      if (_hasNext()) {
        state() = num.rangeRand(loStream.next(), hiStream.next())
      }
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
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

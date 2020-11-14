/*
 *  DifferentiateImpl.scala
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

import de.sciss.lucre.{Adjunct, Exec, Ident, Var}
import de.sciss.lucre.Adjunct.Num
import de.sciss.patterns.graph.Differentiate
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

object DifferentiateImpl extends StreamFactory {
  final val typeId = 0x44696666 // "Diff"

  def expand[T <: Exec[T], A](pat: Differentiate[A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in.expand[T]
    val x1        = PatElem.makeVar[T, A](id)
    val state     = PatElem.makeVar[T, A](id)
    val _hasNext  = id.newBooleanVar(false)
    val valid     = id.newBooleanVar(false)

    new StreamImpl[T, A](id = id, inStream = inStream, x1 = x1, state = state, _hasNext = _hasNext,
      valid = valid)(num)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id        = tx.readId(in)
    val inStream  = Stream.read[T, Any](in)
    val x1        = PatElem.readVar[T, Any](id, in)
    val state     = PatElem.readVar[T, Any](id, in)
    val _hasNext  = id.readBooleanVar(in)
    val valid     = id.readBooleanVar(in)
    val num       = Adjunct.readT[Num[Any]](in)

    new StreamImpl[T, Any](id = id, inStream = inStream, x1 = x1, state = state, _hasNext = _hasNext,
      valid = valid)(num)
  }

  private final class StreamImpl[T <: Exec[T], A](
                                                   id        : Ident[T],
                                                   inStream  : Stream[T, A],
                                                   x1        : Var[T, A],
                                                   state     : Var[T, A],
                                                   _hasNext  : Var[T, Boolean],
                                                   valid     : Var[T, Boolean]
  ) (
    num: Num[A]
  )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut       = txOut.newId()
      val inStreamOut = c(inStream)
      val x1Out       = PatElem.copyVar[Out, A](idOut, x1())
      val stateOut    = PatElem.copyVar[Out, A](idOut, state())
      val hasNextOut  = idOut.newBooleanVar(_hasNext())
      val validOut    = idOut.newBooleanVar(valid())

      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, x1 = x1Out, state = stateOut, _hasNext = hasNextOut,
        valid = validOut)(num)
    }

    protected def typeId: Int = DifferentiateImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id        .write(out)
      inStream  .write(out)
      x1        .write(out)
      state     .write(out)
      _hasNext  .write(out)
      valid     .write(out)
      num       .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id        .dispose()
      inStream  .dispose()
      x1        .dispose()
      state     .dispose()
      _hasNext  .dispose()
      valid     .dispose()
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      inStream.reset()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      if (inStream.hasNext) {
        x1() = inStream.next()
        advance()
      } else {
        _hasNext() = false
      }
    }

    private def advance()(implicit ctx: Context[T], tx: T): Unit = {
      if (inStream.hasNext) {
        val in1     = x1()
        val in0     = inStream.next()
        x1()        = in0
        state()     = num.minus(in0, in1)
        _hasNext()  = true
      } else {
        _hasNext() = false
      }
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
      if (!hasNext) Stream.exhausted()
      val res = state()
      advance()
      res
    }
  }
}

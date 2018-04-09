/*
 *  DifferentiateImpl.scala
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
import de.sciss.patterns.Types.{Aux, Num}
import de.sciss.patterns.graph.Differentiate
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

object DifferentiateImpl extends StreamFactory {
  final val typeId = 0x44696666 // "Diff"

  def expand[S <: Base[S], A](pat: Differentiate[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in.expand[S]
    val x1        = PatElem.makeVar[S, A](id)
    val state     = PatElem.makeVar[S, A](id)
    val _hasNext  = tx.newBooleanVar(id, false)
    val valid     = tx.newBooleanVar(id, false)

    new StreamImpl[S, A](id = id, inStream = inStream, x1 = x1, state = state, _hasNext = _hasNext,
      valid = valid)(num)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    val id        = tx.readId(in, access)
    val inStream  = Stream.read[S, A](in, access)
    val x1        = PatElem.readVar[S, A](id, in)
    val state     = PatElem.readVar[S, A](id, in)
    val _hasNext  = tx.readBooleanVar(id, in)
    val valid     = tx.readBooleanVar(id, in)
    val num       = Aux.readT[Num[A]](in)

    new StreamImpl[S, A](id = id, inStream = inStream, x1 = x1, state = state, _hasNext = _hasNext,
      valid = valid)(num)
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   id        : S#Id,
                                                   inStream  : Stream[S, A],
                                                   x1        : S#Var[A],
                                                   state     : S#Var[A],
                                                   _hasNext  : S#Var[Boolean],
                                                   valid     : S#Var[Boolean]
  ) (
    num       : Num[A]
  )
    extends Stream[S, A] {

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

    def dispose()(implicit tx: S#Tx): Unit = {
      id        .dispose()
      inStream  .dispose()
      x1        .dispose()
      state     .dispose()
      _hasNext  .dispose()
      valid     .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      inStream.reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      if (inStream.hasNext) {
        x1() = inStream.next()
        advance()
      } else {
        _hasNext() = false
      }
    }

    private def advance()(implicit ctx: Context[S], tx: S#Tx): Unit = {
      if (inStream.hasNext) {
        val in1     = x1()
        val in0     = inStream.next()
        x1()        = in0
        state()     = num.-(in0, in1)
        _hasNext()  = true
      } else {
        _hasNext() = false
      }
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = state()
      advance()
      res
    }
  }
}

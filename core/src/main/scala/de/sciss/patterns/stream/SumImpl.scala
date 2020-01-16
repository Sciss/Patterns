/*
 *  SumImpl.scala
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
import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.Sum
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

object SumImpl extends StreamFactory {
  final val typeId = 0x53756D20 // "Sum "

  def expand[S <: Base[S], A](pat: Sum[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in.expand[S]
    val state     = PatElem.makeVar[S, A](id)
    val _hasNext  = tx.newBooleanVar(id, false)
    val valid     = tx.newBooleanVar(id, false)

    new StreamImpl[S, A](id = id, inStream = inStream, state = state, _hasNext = _hasNext, valid = valid)(num)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id        = tx.readId(in, access)
    val inStream  = Stream.read[S, Any](in, access)
    val state     = PatElem.readVar[S, Any](id, in)
    val _hasNext  = tx.readBooleanVar(id, in)
    val valid     = tx.readBooleanVar(id, in)
    val num       = Adjunct.readT[Num[Any]](in)

    new StreamImpl[S, Any](id = id, inStream = inStream, state = state, _hasNext = _hasNext, valid = valid)(num)
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   id      : S#Id,
                                                   inStream: Stream[S, A],
                                                   state   : S#Var[A],
                                                   _hasNext: S#Var[Boolean],
                                                   valid   : S#Var[Boolean]
  )(
    implicit num: Num[A]
  )
    extends Stream[S, A] {

    private[patterns] def copyStream[Out <: Base[Out]](c: Stream.Copy[S, Out])
                                                      (implicit tx: S#Tx, txOut: Out#Tx): Stream[Out, A] = {
      val idOut       = txOut.newId()
      val inStreamOut = c(inStream)
      val stateOut    = PatElem.copyVar[Out, A](idOut, state())
      val hasNextOut  = txOut.newBooleanVar(idOut, _hasNext())
      val validOut    = txOut.newBooleanVar(idOut, valid())

      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, state = stateOut,
        _hasNext = hasNextOut, valid = validOut)(num)
    }

    protected def typeId: Int = SumImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      inStream.write(out)
      state   .write(out)
      _hasNext.write(out)
      valid   .write(out)
      num     .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id      .dispose()
      inStream.dispose()
      state   .dispose()
      _hasNext.dispose()
      valid   .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      inStream.reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      val ihn = inStream.hasNext
      _hasNext() = ihn
      if (ihn) {
        var acc = inStream.next()
        while (inStream.hasNext) {
          acc = num.+(acc, inStream.next())
        }
        state() = acc
      }
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = state()
      _hasNext() = false
      res
    }
  }
}

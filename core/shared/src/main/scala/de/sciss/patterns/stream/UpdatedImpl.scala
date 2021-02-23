/*
 *  UpdatedImpl.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package stream

import de.sciss.lucre.{Exec, Ident, Var}
import de.sciss.patterns.graph.Updated
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

object UpdatedImpl extends StreamFactory {
  final val typeId = 0x55706461 // "Upda"

  def expand[T <: Exec[T], A1, A >: A1](pat: Updated[A1, A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in  .expand[T]
    val idxStream   = idx .expand[T]
    val takeRem     = id.newIntVar(0)
    val _hasNext    = id.newBooleanVar(false)
    val valid       = id.newBooleanVar(false)

    new StreamImpl[T, A1, A](id = id, inStream = inStream, idxStream = idxStream, takeRem = takeRem,
      _hasNext = _hasNext, valid = valid, elem = elem)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id          = tx.readId(in)
    val inStream    = Stream.read[T, Any](in)
    val idxStream   = Stream.read[T, Int](in)
    val takeRem     = id.readIntVar(in)
    val _hasNext    = id.readBooleanVar(in)
    val valid       = id.readBooleanVar(in)
    val elem        = PatElem.read[Any](in)

    new StreamImpl[T, Any, Any](id = id, inStream = inStream, idxStream = idxStream, takeRem = takeRem,
      _hasNext = _hasNext, valid = valid, elem = elem)
  }

  private final class StreamImpl[T <: Exec[T], A1, A >: A1](
                                                             id        : Ident[T],
                                                             inStream  : Stream[T, A1],
                                                             idxStream : Stream[T, Int],
                                                             takeRem   : Var[T, Int],
                                                             _hasNext  : Var[T, Boolean],
                                                             valid     : Var[T, Boolean],
                                                             elem      : A
  )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut         = txOut.newId()
      val inStreamOut   = c(inStream )
      val idxStreamOut  = c(idxStream)
      val takeRemOut    = idOut.newIntVar(takeRem())
      val hasNextOut    = idOut.newBooleanVar(_hasNext())
      val validOut      = idOut.newBooleanVar(valid())

      new StreamImpl[Out, A1, A](id = idOut, inStream = inStreamOut, idxStream = idxStreamOut, takeRem = takeRemOut,
        _hasNext = hasNextOut, valid = validOut, elem = elem)
    }

    protected def typeId: Int = UpdatedImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id        .write(out)
      inStream  .write(out)
      idxStream .write(out)
      takeRem   .write(out)
      _hasNext  .write(out)
      valid     .write(out)
      PatElem.write(elem, out)
    }

    def dispose()(implicit tx: T): Unit = {
      id        .dispose()
      inStream  .dispose()
      idxStream .dispose()
      takeRem   .dispose()
      _hasNext  .dispose()
      valid     .dispose()
    }

    def reset()(implicit tx: T): Unit = if (valid()) {
      valid() = false
      inStream  .reset()
      idxStream .reset()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid()) {
      valid()  = true
      val xhn   = idxStream.hasNext
      if (xhn) {
        val _idxVal = idxStream.next()
        takeRem()   = _idxVal
        _hasNext()  = _idxVal >= 0 && inStream.hasNext
      } else {
        _hasNext()  = false
      }
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
      if (!hasNext) Stream.exhausted()
      val c     = takeRem()
      val inVal = inStream.next()
      val res   = if (c == 0) elem else inVal
      if (c > 0) takeRem() = c - 1
      if (!inStream.hasNext) _hasNext() = false
      res
    }
  }
}

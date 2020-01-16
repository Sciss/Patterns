/*
 *  UpdatedImpl.scala
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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.Updated
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

object UpdatedImpl extends StreamFactory {
  final val typeId = 0x55706461 // "Upda"

  def expand[S <: Base[S], A1, A >: A1](pat: Updated[A1, A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in  .expand[S]
    val idxStream   = idx .expand[S]
    val takeRem     = tx.newIntVar(id, 0)
    val _hasNext    = tx.newBooleanVar(id, false)
    val valid       = tx.newBooleanVar(id, false)

    new StreamImpl[S, A1, A](id = id, inStream = inStream, idxStream = idxStream, takeRem = takeRem,
      _hasNext = _hasNext, valid = valid, elem = elem)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id          = tx.readId(in, access)
    val inStream    = Stream.read[S, Any](in, access)
    val idxStream   = Stream.read[S, Int](in, access)
    val takeRem     = tx.readIntVar(id, in)
    val _hasNext    = tx.readBooleanVar(id, in)
    val valid       = tx.readBooleanVar(id, in)
    val elem        = PatElem.read[Any](in)

    new StreamImpl[S, Any, Any](id = id, inStream = inStream, idxStream = idxStream, takeRem = takeRem,
      _hasNext = _hasNext, valid = valid, elem = elem)
  }

  private final class StreamImpl[S <: Base[S], A1, A >: A1](
                                                             id        : S#Id,
                                                             inStream  : Stream[S, A1],
                                                             idxStream : Stream[S, Int],
                                                             takeRem   : S#Var[Int],
                                                             _hasNext  : S#Var[Boolean],
                                                             valid     : S#Var[Boolean],
                                                             elem      : A
  )
    extends Stream[S, A] {

    private[patterns] def copyStream[Out <: Base[Out]]()(implicit tx: S#Tx, txOut: Out#Tx,
                                                         ctx: Context[Out]): Stream[Out, A] = {
      val idOut         = txOut.newId()
      val inStreamOut   = inStream  .copyStream[Out]()
      val idxStreamOut  = idxStream .copyStream[Out]()
      val takeRemOut    = txOut.newIntVar(idOut, takeRem())
      val hasNextOut    = txOut.newBooleanVar(idOut, _hasNext())
      val validOut      = txOut.newBooleanVar(idOut, valid())

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

    def dispose()(implicit tx: S#Tx): Unit = {
      id        .dispose()
      inStream  .dispose()
      idxStream .dispose()
      takeRem   .dispose()
      _hasNext  .dispose()
      valid     .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid()) {
      valid() = false
      inStream  .reset()
      idxStream .reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid()) {
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

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
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

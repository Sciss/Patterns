/*
 *  HoldImpl.scala
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
import de.sciss.patterns.graph.Hold
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

object HoldImpl extends StreamFactory {
  final val typeId = 0x486F6C64 // "Hold"

  def expand[T <: Exec[T], A](pat: Hold[A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in  .expand[T]
    val holdStream  = hold.expand[T]
    val hasIn       = id.newBooleanVar(false)
    val state       = PatElem.makeVar[T, A](id)
    val _hasNext    = id.newBooleanVar(false)
    val valid       = id.newBooleanVar(false)

    new StreamImpl[T, A](id = id, inStream = inStream, holdStream = holdStream, hasIn = hasIn, state = state,
      _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id          = tx.readId(in)
    val inStream    = Stream.read[T, Any      ](in)
    val holdStream  = Stream.read[T, Boolean](in)
    val hasIn       = id.readBooleanVar(in)
    val state       = PatElem.readVar[T, Any](id, in)
    val _hasNext    = id.readBooleanVar(in)
    val valid       = id.readBooleanVar(in)

    new StreamImpl[T, Any](id = id, inStream = inStream, holdStream = holdStream, hasIn = hasIn, state = state,
      _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[T <: Exec[T], A](
                                                   id        : Ident[T],
                                                   inStream  : Stream[T, A],
                                                   holdStream: Stream[T, Boolean],
                                                   hasIn     : Var[T, Boolean],
                                                   state     : Var[T, A],
                                                   _hasNext  : Var[T, Boolean],
                                                   valid     : Var[T, Boolean]
  )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut         = txOut.newId()
      val inStreamOut   = c(inStream  )
      val holdStreamOut = c(holdStream)
      val hasInOut      = idOut.newBooleanVar(hasIn())
      val stateOut      = PatElem.copyVar[Out, A](idOut, state())
      val hasNextOut    = idOut.newBooleanVar(_hasNext())
      val validOut      = idOut.newBooleanVar(valid())

      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, holdStream = holdStreamOut, hasIn = hasInOut,
        state = stateOut, _hasNext = hasNextOut, valid = validOut)
    }

    protected def typeId: Int = HoldImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id        .write(out)
      inStream  .write(out)
      holdStream.write(out)
      hasIn     .write(out)
      state     .write(out)
      _hasNext  .write(out)
      valid     .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id        .dispose()
      inStream  .dispose()
      holdStream.dispose()
      hasIn     .dispose()
      state     .dispose()
      _hasNext  .dispose()
      valid     .dispose()
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      inStream  .reset()
      holdStream.reset()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      hasIn()  = false
      advance()
    }

    private def advance()(implicit ctx: Context[T], tx: T): Unit = {
      val hhn     = holdStream.hasNext
      _hasNext()  = hhn
      if (hhn) {
        val holdVal = holdStream.next()
        if (!holdVal) hasIn() = false  // 'flush'

        if (!hasIn()) {
          val ihn = inStream.hasNext
          if (ihn) {
            hasIn()     = true
            val inVal   = inStream.next()
            state()     = inVal
          } else {
            _hasNext()  = false
          }
        }
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

/*
 *  HoldImpl.scala
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
import de.sciss.patterns.graph.Hold
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

object HoldImpl extends StreamFactory {
  final val typeId = 0x486F6C64 // "Hold"

  def expand[S <: Base[S], A](pat: Hold[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in  .expand[S]
    val holdStream  = hold.expand[S]
    val hasIn       = tx.newBooleanVar(id, false)
    val state       = PatElem.makeVar[S, A](id)
    val _hasNext    = tx.newBooleanVar(id, false)
    val valid       = tx.newBooleanVar(id, false)

    new StreamImpl[S, A](id = id, inStream = inStream, holdStream = holdStream, hasIn = hasIn, state = state,
      _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id          = tx.readId(in, access)
    val inStream    = Stream.read[S, Any      ](in, access)
    val holdStream  = Stream.read[S, Boolean](in, access)
    val hasIn       = tx.readBooleanVar(id, in)
    val state       = PatElem.readVar[S, Any](id, in)
    val _hasNext    = tx.readBooleanVar(id, in)
    val valid       = tx.readBooleanVar(id, in)

    new StreamImpl[S, Any](id = id, inStream = inStream, holdStream = holdStream, hasIn = hasIn, state = state,
      _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   id        : S#Id,
                                                   inStream  : Stream[S, A],
                                                   holdStream: Stream[S, Boolean],
                                                   hasIn     : S#Var[Boolean],
                                                   state     : S#Var[A],
                                                   _hasNext  : S#Var[Boolean],
                                                   valid     : S#Var[Boolean]
  )
    extends Stream[S, A] {

    private[patterns] def copyStream[Out <: Base[Out]](c: Stream.Copy[S, Out])
                                                      (implicit tx: S#Tx, txOut: Out#Tx): Stream[Out, A] = {
      val idOut         = txOut.newId()
      val inStreamOut   = c(inStream  )
      val holdStreamOut = c(holdStream)
      val hasInOut      = txOut.newBooleanVar(idOut, hasIn())
      val stateOut      = PatElem.copyVar[Out, A](idOut, state())
      val hasNextOut    = txOut.newBooleanVar(idOut, _hasNext())
      val validOut      = txOut.newBooleanVar(idOut, valid())

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

    def dispose()(implicit tx: S#Tx): Unit = {
      id        .dispose()
      inStream  .dispose()
      holdStream.dispose()
      hasIn     .dispose()
      state     .dispose()
      _hasNext  .dispose()
      valid     .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      inStream  .reset()
      holdStream.reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      hasIn()  = false
      advance()
    }

    private def advance()(implicit ctx: Context[S], tx: S#Tx): Unit = {
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

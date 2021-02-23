/*
 *  GateImpl.scala
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
import de.sciss.patterns.graph.Gate
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.tailrec

object GateImpl extends StreamFactory {
  final val typeId = 0x47617465 // "Gate"

  def expand[T <: Exec[T], A](pat: Gate[A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in  .expand[T]
    val gateStream  = gate.expand[T]
    val _next       = PatElem.makeVar[T, A](id)
    val _hasNext    = id.newBooleanVar(false)
    val valid       = id.newBooleanVar(false)

    new StreamImpl[T, A](id = id, inStream = inStream, gateStream = gateStream, _next = _next,
      _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id          = tx.readId(in)
    val inStream    = Stream.read[T, Any    ](in)
    val gateStream  = Stream.read[T, Boolean](in)
    val _next       = PatElem.readVar[T, Any](id, in)
    val _hasNext    = id.readBooleanVar(in)
    val valid       = id.readBooleanVar(in)

    new StreamImpl[T, Any](id = id, inStream = inStream, gateStream = gateStream, _next = _next,
      _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[T <: Exec[T], A](
                                                   private[this] val id        : Ident[T],
                                                   private[this] val inStream  : Stream[T, A],
                                                   private[this] val gateStream: Stream[T, Boolean],
                                                   private[this] val _next     : Var[T, A],
                                                   private[this] val _hasNext  : Var[T, Boolean],
                                                   private[this] val valid     : Var[T, Boolean]
  )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut         = txOut.newId()
      val inStreamOut   = c(inStream)
      val gateStreamOut = c(gateStream)
      val nextOut       = PatElem.copyVar[Out, A](idOut, _next())
      val hasNextOut    = idOut.newBooleanVar(_hasNext())
      val validOut      = idOut.newBooleanVar(valid())

      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, gateStream = gateStreamOut, _next = nextOut,
        _hasNext = hasNextOut, valid = validOut)
    }

    protected def typeId: Int = GateImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id        .write(out)
      inStream  .write(out)
      gateStream.write(out)
      _next     .write(out)
      _hasNext  .write(out)
      valid     .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id        .dispose()
      inStream  .dispose()
      gateStream.dispose()
      _next     .dispose()
      _hasNext  .dispose()
      valid     .dispose()
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      inStream  .reset()
      gateStream.reset()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      advance()
    }

    @tailrec
    private def advance()(implicit ctx: Context[T], tx: T): Unit = {
      if (inStream.hasNext) {
        val inVal = inStream.next()
        if (gateStream.hasNext) {
          val gateVal = gateStream.next()
          if (gateVal) {
            _next() = inVal
            _hasNext()  = true
          } else {
            advance()
          }
        } else {
          _hasNext() = false
        }
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
      val res = _next()
      advance()
      res
    }
  }
}

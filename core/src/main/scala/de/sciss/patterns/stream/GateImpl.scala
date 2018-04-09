/*
 *  GateImpl.scala
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
import de.sciss.patterns.graph.Gate
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.tailrec

object GateImpl extends StreamFactory {
  final val typeId = 0x47617465 // "Gate"

  def expand[S <: Base[S], A](pat: Gate[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in  .expand[S]
    val gateStream  = gate.expand[S]
    val _next       = PatElem.makeVar[S, A](id)
    val _hasNext    = tx.newBooleanVar(id, false)
    val valid       = tx.newBooleanVar(id, false)

    new StreamImpl[S, A](id = id, inStream = inStream, gateStream = gateStream, _next = _next,
      _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id          = tx.readId(in, access)
    val inStream    = Stream.read[S, Any    ](in, access)
    val gateStream  = Stream.read[S, Boolean](in, access)
    val _next       = PatElem.readVar[S, Any](id, in)
    val _hasNext    = tx.readBooleanVar(id, in)
    val valid       = tx.readBooleanVar(id, in)

    new StreamImpl[S, Any](id = id, inStream = inStream, gateStream = gateStream, _next = _next,
      _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   private[this] val id        : S#Id,
                                                   private[this] val inStream  : Stream[S, A],
                                                   private[this] val gateStream: Stream[S, Boolean],
                                                   private[this] val _next     : S#Var[A],
                                                   private[this] val _hasNext  : S#Var[Boolean],
                                                   private[this] val valid     : S#Var[Boolean]
  )
    extends Stream[S, A] {

    protected def typeId: Int = GateImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id        .write(out)
      inStream  .write(out)
      gateStream.write(out)
      _next     .write(out)
      _hasNext  .write(out)
      valid     .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id        .dispose()
      inStream  .dispose()
      gateStream.dispose()
      _next     .dispose()
      _hasNext  .dispose()
      valid     .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      inStream  .reset()
      gateStream.reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      advance()
    }

    @tailrec
    private def advance()(implicit ctx: Context[S], tx: S#Tx): Unit = {
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

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = _next()
      advance()
      res
    }
  }
}

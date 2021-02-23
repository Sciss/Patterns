/*
 *  DistinctImpl.scala
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
import de.sciss.patterns.graph.Distinct
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.tailrec

object DistinctImpl extends StreamFactory {
  final val typeId = 0x44697374 // "Dist"

  def expand[T <: Exec[T], A](pat: Distinct[A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in.expand[T]
    val seen      = id.newVar[Set[A]](Set.empty)(tx, PatElem.setFormat)
    val _next     = PatElem.makeVar[T, A](id)
    val _hasNext  = id.newBooleanVar(false)
    val valid     = id.newBooleanVar(false)

    new StreamImpl[T, A](id = id, inStream = inStream, seen = seen, _next = _next, _hasNext = _hasNext,
      valid = valid)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id        = tx.readId(in)
    val inStream  = Stream.read[T, Any](in)
    val seen      = id.readVar[Set[Any]](in)(PatElem.setFormat)
    val _next     = PatElem.readVar[T, Any](id, in)
    val _hasNext  = id.readBooleanVar(in)
    val valid     = id.readBooleanVar(in)

    new StreamImpl[T, Any](id = id, inStream = inStream, seen = seen, _next = _next, _hasNext = _hasNext,
      valid = valid)
  }

  private final class StreamImpl[T <: Exec[T], A](
                                                   id      : Ident[T],
                                                   inStream: Stream[T, A],
                                                   seen    : Var[T, Set[A]],
                                                   _next   : Var[T, A],
                                                   _hasNext: Var[T, Boolean],
                                                   valid   : Var[T, Boolean]
  ) extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut       = txOut.newId()
      val inStreamOut = c(inStream)
      val seenOut     = idOut.newVar[Set[A]](seen())(txOut, PatElem.setFormat)
      val nextOut     = PatElem.copyVar[Out, A](idOut, _next())
      val hasNextOut  = idOut.newBooleanVar(_hasNext())
      val validOut    = idOut.newBooleanVar(valid())

      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, seen = seenOut, _next = nextOut, _hasNext = hasNextOut,
        valid = validOut)
    }

    protected def typeId: Int = DistinctImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      inStream.write(out)
      seen    .write(out)
      _next   .write(out)
      _hasNext.write(out)
      valid   .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id      .dispose()
      inStream.dispose()
      seen    .dispose()
      _next   .dispose()
      _hasNext.dispose()
      valid   .dispose()
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      inStream.reset()
    }

    @tailrec
    private def advance()(implicit ctx: Context[T], tx: T): Unit = {
      _hasNext() = inStream.hasNext
      if (_hasNext()) {
        _next()     = inStream.next()
        _hasNext()  = !seen().contains(_next())
        if (_hasNext()) {
          seen() = seen() + _next()
        } else {
          advance()
        }
      }
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      seen() = Set.empty
      advance()
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = _next()
      advance()
      res
    }
  }
}

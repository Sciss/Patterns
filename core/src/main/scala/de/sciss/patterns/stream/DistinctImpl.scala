/*
 *  DistinctImpl.scala
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
import de.sciss.patterns.graph.Distinct
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.tailrec

object DistinctImpl extends StreamFactory {
  final val typeId = 0x44697374 // "Dist"

  def expand[S <: Base[S], A](pat: Distinct[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in.expand[S]
    val seen      = tx.newVar[Set[A]](id, Set.empty)(PatElem.setSerializer)
    val _next     = PatElem.makeVar[S, A](id)
    val _hasNext  = tx.newBooleanVar(id, false)
    val valid     = tx.newBooleanVar(id, false)

    new StreamImpl[S, A](id = id, inStream = inStream, seen = seen, _next = _next, _hasNext = _hasNext,
      valid = valid)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id        = tx.readId(in, access)
    val inStream  = Stream.read[S, Any](in, access)
    val seen      = tx.readVar[Set[Any]](id, in)(PatElem.setSerializer)
    val _next     = PatElem.readVar[S, Any](id, in)
    val _hasNext  = tx.readBooleanVar(id, in)
    val valid     = tx.readBooleanVar(id, in)

    new StreamImpl[S, Any](id = id, inStream = inStream, seen = seen, _next = _next, _hasNext = _hasNext,
      valid = valid)
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   id      : S#Id,
                                                   inStream: Stream[S, A],
                                                   seen    : S#Var[Set[A]],
                                                   _next   : S#Var[A],
                                                   _hasNext: S#Var[Boolean],
                                                   valid   : S#Var[Boolean]
  ) extends Stream[S, A] {

    protected def typeId: Int = DistinctImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      inStream.write(out)
      seen    .write(out)
      _next   .write(out)
      _hasNext.write(out)
      valid   .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id      .dispose()
      inStream.dispose()
      seen    .dispose()
      _next   .dispose()
      _hasNext.dispose()
      valid   .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      inStream.reset()
    }

    @tailrec
    private def advance()(implicit ctx: Context[S], tx: S#Tx): Unit = {
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

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      seen() = Set.empty
      advance()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = _next()
      advance()
      res
    }
  }
}

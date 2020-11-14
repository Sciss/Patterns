/*
 *  FlattenImpl.scala
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

import de.sciss.lucre.{Exec, Ident, Var}
import de.sciss.patterns.graph.Flatten
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.tailrec

object FlattenImpl extends StreamFactory {
  final val typeId = 0x466C6174 // "Flat"

  def expand[T <: Exec[T], A](pat: Flatten[A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in.expand[T]
    val hasInner    = id.newBooleanVar(false)
    val innerStream = id.newVar[Stream[T, A]](null)
    val _hasNext    = id.newBooleanVar(false)
    val valid       = id.newBooleanVar(false)

    new StreamImpl[T, A](id = id, inStream = inStream, hasInner = hasInner, innerStream = innerStream,
      _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id          = tx.readId(in)
    val inStream    = Stream.read[T, Pat[Any]](in)
    val hasInner    = id.readBooleanVar(in)
    val innerStream = id.readVar[Stream[T, Any]](in)
    val _hasNext    = id.readBooleanVar(in)
    val valid       = id.readBooleanVar(in)

    new StreamImpl[T, Any](id = id, inStream = inStream, hasInner = hasInner, innerStream = innerStream,
      _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[T <: Exec[T], A](
                                                   id          : Ident[T],
                                                   inStream    : Stream[T, Pat[A]],
                                                   hasInner    : Var[T, Boolean],
                                                   innerStream : Var[T, Stream[T, A]],
                                                   _hasNext    : Var[T, Boolean],
                                                   valid       : Var[T, Boolean]
  )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut           = txOut.newId()
      val inStreamOut     = c(inStream)
      val hasInnerOut     = idOut.newBooleanVar(hasInner())
      val innerStreamOut  = c.copyVar(idOut, innerStream)
      val hasNextOut      = idOut.newBooleanVar(_hasNext())
      val validOut        = idOut.newBooleanVar(valid())

      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, hasInner = hasInnerOut, innerStream = innerStreamOut,
        _hasNext = hasNextOut, valid = validOut)
    }

    protected def typeId: Int = FlattenImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id          .write(out)
      inStream    .write(out)
      hasInner    .write(out)
      innerStream .write(out)
      _hasNext    .write(out)
      valid       .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id          .dispose()
      inStream    .dispose()
      hasInner    .dispose()
      innerStream .dispose()
      _hasNext    .dispose()
      valid       .dispose()
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      inStream.reset()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      hasInner()  = false
      advance()
    }

    @tailrec
    private def advance()(implicit ctx: Context[T], tx: T): Unit = {
      if (hasInner()) {
        hasInner() = innerStream().hasNext
      }

      _hasNext() = hasInner()
      if (!_hasNext()) {
        _hasNext() = inStream.hasNext
        if (_hasNext()) {
          val inPat     = inStream.next()
          innerStream() = inPat.expand
          hasInner() = true
          advance()
        }
      }
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
      if (!hasNext) Stream.exhausted()
      val res = innerStream().next()
      advance()
      res
    }
  }
}

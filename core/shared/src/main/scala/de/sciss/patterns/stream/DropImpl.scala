/*
 *  DropImpl.scala
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
import de.sciss.patterns.graph.Drop
import de.sciss.patterns.stream.impl.TruncateLikeStreamImpl
import de.sciss.serial.{DataInput, DataOutput}

object DropImpl extends StreamFactory {
  final val typeId = 0x44726F70 // "Drop

  def expand[T <: Exec[T], A](pat: Drop[A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in    .expand[T]
    val lenStream = length.expand[T]
    val _hasNext  = id.newBooleanVar(false)
    val valid     = id.newBooleanVar(false)

    new StreamImpl[T, A](id = id, inStream = inStream, lenStream = lenStream, _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id        = tx.readId(in)
    val inStream  = Stream.read[T, Any](in)
    val lenStream = Stream.read[T, Int](in)
    val _hasNext  = id.readBooleanVar(in)
    val valid     = id.readBooleanVar(in)

    new StreamImpl[T, Any](id = id, inStream = inStream, lenStream = lenStream, _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[T <: Exec[T], A](
                                                   protected val id        : Ident[T],
                                                   protected val inStream  : Stream[T, A],
                                                   protected val lenStream : Stream[T, Int],
                                                   protected val _hasNext  : Var[T, Boolean],
                                                   protected val valid     : Var[T, Boolean]
  )
    extends TruncateLikeStreamImpl[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut         = txOut.newId()
      val inStreamOut   = c(inStream )
      val lenStreamOut  = c(lenStream)
      val hasNextOut    = idOut.newBooleanVar(_hasNext())
      val validOut      = idOut.newBooleanVar(valid())

      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, lenStream = lenStreamOut,
        _hasNext = hasNextOut, valid = validOut)
    }

    protected def typeId: Int = DropImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id        .write(out)
      inStream  .write(out)
      lenStream .write(out)
      _hasNext  .write(out)
      valid     .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id        .dispose()
      inStream  .dispose()
      lenStream .dispose()
      _hasNext  .dispose()
      valid     .dispose()
    }

    protected def validateWithLen(n: Int)(implicit ctx: Context[T], tx: T): Boolean = {
      var i = 0
      while (i < n && inStream.hasNext) {
        inStream.next()
        i += 1
      }
      inStream.hasNext
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
      if (!hasNext) Stream.exhausted()
      val res = inStream.next()
      if (!inStream.hasNext) _hasNext() = false
      res
    }
  }
}

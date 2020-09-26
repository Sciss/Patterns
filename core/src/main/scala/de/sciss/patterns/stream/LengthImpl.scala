/*
 *  LengthImpl.scala
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
import de.sciss.patterns.graph.Length
import de.sciss.serial.{DataInput, DataOutput}

object LengthImpl extends StreamFactory {
  final val typeId = 0x4C656E67 // "Leng"

  def expand[T <: Exec[T], A](pat: Length[A])(implicit ctx: Context[T], tx: T): Stream[T, Int] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in.expand[T]
    val _hasNext  = id.newBooleanVar(true)
    new StreamImpl[T, A](id = id, inStream = inStream, _hasNext = _hasNext)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id        = tx.readId(in)
    val inStream  = Stream.read[T, Any](in)
    val _hasNext  = id.readBooleanVar(in)

    new StreamImpl[T, Any](id = id, inStream = inStream, _hasNext = _hasNext)
  }

  private final class StreamImpl[T <: Exec[T], A](
                                                   id      : Ident[T],
                                                   inStream: Stream[T, A],
                                                   _hasNext: Var[T, Boolean]
  )
    extends Stream[T, Int] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, Int] = {
      val idOut       = txOut.newId()
      val inStreamOut = c(inStream)
      val hasNextOut  = idOut.newBooleanVar(_hasNext())
      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, _hasNext = hasNextOut)
    }

    protected def typeId: Int = LengthImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      inStream.write(out)
      _hasNext.write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id      .dispose()
      inStream.dispose()
      _hasNext.dispose()
    }

    def reset()(implicit tx: T): Unit = {
      inStream.reset()
      _hasNext() = true
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = _hasNext()

    def next()(implicit ctx: Context[T], tx: T): Int = {
      if (!hasNext) Stream.exhausted()
      var res = 0
      while (inStream.hasNext) {
        inStream.next()
        res += 1
      }
      _hasNext() = false
      res
    }
  }

}

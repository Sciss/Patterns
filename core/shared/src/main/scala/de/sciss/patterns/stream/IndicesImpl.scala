/*
 *  IndicesImpl.scala
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
import de.sciss.patterns.graph.Indices
import de.sciss.serial.{DataInput, DataOutput}

object IndicesImpl extends StreamFactory {
  final val typeId = 0x496E6469 // "Indi"

  def expand[T <: Exec[T], A](pat: Indices[A])(implicit ctx: Context[T], tx: T): Stream[T, Int] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in.expand[T]
    val count     = id.newIntVar(0)
    new StreamImpl[T, A](id = id, inStream = inStream, count = count)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id        = tx.readId(in)
    val inStream  = Stream.read[T, Any](in)
    val count     = id.readIntVar(in)

    new StreamImpl[T, Any](id = id, inStream = inStream, count = count)
  }

  private final class StreamImpl[T <: Exec[T], A](
                                                   id      : Ident[T],
                                                   inStream: Stream[T, A],
                                                   count   : Var[T, Int]
  )
    extends Stream[T, Int] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, Int] = {
      val idOut       = txOut.newId()
      val inStreamOut = c(inStream)
      val countOut    = idOut.newIntVar(count())
      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, count = countOut)
    }

    protected def typeId: Int = IndicesImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      inStream.write(out)
      count   .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id      .dispose()
      inStream.dispose()
      count   .dispose()
    }

    def reset()(implicit tx: T): Unit = {
      inStream.reset()
      count() = 0
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean =
      inStream.hasNext

    def next()(implicit ctx: Context[T], tx: T): Int = {
      val res = count()
      inStream.next()
      count() = res + 1
      res
    }
  }
}

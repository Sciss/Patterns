/*
 *  Zip2Impl.scala
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

import de.sciss.lucre.Exec
import de.sciss.patterns.graph.Zip2
import de.sciss.serial.{DataInput, DataOutput}

object Zip2Impl extends StreamFactory {
  final val typeId = 0x5A697032 // "Zip2"

  def expand[T <: Exec[T], A1, A2](pat: Zip2[A1, A2])(implicit ctx: Context[T], tx: T): Stream[T, (A1, A2)] = {
    import pat._
    val aStream     = a.expand(ctx, tx)
    val bStream     = b.expand(ctx, tx)
    new StreamImpl[T, A1, A2](aStream = aStream, bStream = bStream)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val aStream     = Stream.read[T, Any](in)
    val bStream     = Stream.read[T, Any](in)

    new StreamImpl[T, Any, Any](aStream = aStream, bStream = bStream)
  }


  private final class StreamImpl[T <: Exec[T], A1, A2](
                                                        val aStream: Stream[T, A1],
                                                        val bStream: Stream[T, A2]
  )
    extends Stream[T, (A1, A2)] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, (A1, A2)] = {
      val aStreamOut = c(aStream)
      val bStreamOut = c(bStream)
      new StreamImpl[Out, A1, A2](aStream = aStreamOut, bStream = bStreamOut)
    }

    protected def typeId: Int = Zip2Impl.typeId

    protected def writeData(out: DataOutput): Unit = {
      aStream.write(out)
      bStream.write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      aStream.dispose()
      bStream.dispose()
    }

    def reset()(implicit tx: T): Unit = {
      aStream.reset()
      bStream.reset()
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = aStream.hasNext && bStream.hasNext

    def next()(implicit ctx: Context[T], tx: T): (A1, A2) = (aStream.next(), bStream.next())
  }
}
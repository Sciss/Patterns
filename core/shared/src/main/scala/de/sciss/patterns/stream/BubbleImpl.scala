/*
 *  BubbleImpl.scala
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
import de.sciss.patterns.graph.{Bubble, Pat}
import de.sciss.serial.{DataInput, DataOutput}

object BubbleImpl extends StreamFactory {
  final val typeId = 0x42756262 // "Bubb"

  def expand[T <: Exec[T], A](pat: Bubble[A])(implicit ctx: Context[T], tx: T): Stream[T, Pat[A]] = {
    import pat._
    val inStream = in.expand[T]
    new StreamImpl[T, A](inStream = inStream)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val inStream = Stream.read[T, Any](in)
    new StreamImpl[T, Any](inStream = inStream)
  }

  private final class StreamImpl[T <: Exec[T], A](inStream: Stream[T, A])
    extends Stream[T, Pat[A]] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, Pat[A]] = {
      val inStreamOut = c(inStream)
      new StreamImpl[Out, A](inStream = inStreamOut)
    }

    protected def typeId: Int = BubbleImpl.typeId

    protected def writeData(out: DataOutput): Unit =
      inStream.write(out)

    def dispose()(implicit tx: T): Unit =
      inStream.dispose()

    def reset()(implicit tx: T): Unit =
      inStream.reset()


    def hasNext(implicit ctx: Context[T], tx: T): Boolean =
      inStream.hasNext

    def next()(implicit ctx: Context[T], tx: T): Pat[A] = {
      val inVal = inStream.next()
      Pat(inVal)
    }
  }
}

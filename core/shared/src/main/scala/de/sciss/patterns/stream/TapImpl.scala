/*
 *  TapImpl.scala
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
import de.sciss.patterns.graph.Tap
import de.sciss.serial.{DataInput, DataOutput}

object TapImpl extends StreamFactory {
  final val typeId = 0x54617020 // "Tap "

  def expand[T <: Exec[T], A, A1](pat: Tap[A, A1])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val inStream    = in  .expand[T]
    val sideStream  = side.expand[T]
    new StreamImpl[T, A, A1](inStream = inStream, sideStream = sideStream)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val inStream    = Stream.read[T, Any  ](in)
    val sideStream  = Stream.read[T, Any](in)
    new StreamImpl[T, Any, Any](inStream = inStream, sideStream = sideStream)
  }

  private final class StreamImpl[T <: Exec[T], A, A1](
                                                       inStream  : Stream[T, A],
                                                       sideStream: Stream[T, A1]
  )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val inStreamOut    = c(inStream  )
      val sideStreamOut  = c(sideStream)
      new StreamImpl[Out, A, A1](inStream = inStreamOut, sideStream = sideStreamOut)
    }

    protected def typeId: Int = TapImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      inStream  .write(out)
      sideStream.write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      inStream  .dispose()
      sideStream.dispose()
    }

    def reset()(implicit tx: T): Unit = {
      inStream  .reset()
      sideStream.reset()
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean =
      inStream.hasNext

    def next()(implicit ctx: Context[T], tx: T): A = {
      val res = inStream.next()
      if (sideStream.hasNext) sideStream.next()
      res
    }
  }
}

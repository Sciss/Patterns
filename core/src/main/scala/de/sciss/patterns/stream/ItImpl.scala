/*
 *  ItImpl.scala
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
import de.sciss.patterns.graph.It
import de.sciss.serial.{DataInput, DataOutput}

object ItImpl extends StreamFactory {
  final val typeId = 0x49742020 // "It  "

  def expand[T <: Exec[T], A](pat: It[A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val refStream = ctx.mkItStream(token)
    new StreamImpl[T, A](refStream = refStream)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val refStream  = Stream.read[T, Any](in)
    new StreamImpl[T, Any](refStream = refStream)
  }


  private final class StreamImpl[T <: Exec[T], A](refStream: Stream[T, A])
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val refStreamOut = c(refStream)
      new StreamImpl[Out, A](refStream = refStreamOut)
    }

    protected def typeId: Int = ItImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      refStream.write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      refStream.dispose()
    }

    def reset()(implicit tx: T): Unit = refStream.reset()

    def hasNext(implicit ctx: Context[T], tx: T): Boolean   = refStream.hasNext
    def next ()(implicit ctx: Context[T], tx: T): A         = refStream.next()
  }
}

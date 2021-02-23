/*
 *  PollImpl.scala
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

import de.sciss.lucre.Exec
import de.sciss.patterns.graph.Poll
import de.sciss.serial.{DataInput, DataOutput}

object PollImpl extends StreamFactory {
  final val typeId = 0x506F6C6C // "Poll"

  def expand[T <: Exec[T], A](pat: Poll[A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val inStream    = in    .expand[T]
    val gateStream  = gate  .expand[T]
    val labelStream = label .expand[T]
    new StreamImpl[T, A](inStream = inStream, gateStream = gateStream, labelStream = labelStream)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val inStream    = Stream.read[T, Any    ](in)
    val gateStream  = Stream.read[T, Boolean](in)
    val labelStream = Stream.read[T, String ](in)

    new StreamImpl[T, Any](inStream = inStream, gateStream = gateStream, labelStream = labelStream)
  }

  private final class StreamImpl[T <: Exec[T], A](
                                                   inStream    : Stream[T, A],
                                                   gateStream  : Stream[T, Boolean],
                                                   labelStream : Stream[T, String]
  )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val inStreamOut    = c(inStream   )
      val gateStreamOut  = c(gateStream )
      val labelStreamOut = c(labelStream)
      new StreamImpl[Out, A](inStream = inStreamOut, gateStream = gateStreamOut, labelStream = labelStreamOut)
    }

    protected def typeId: Int = PollImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      inStream    .write(out)
      gateStream  .write(out)
      labelStream .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      inStream    .dispose()
      gateStream  .dispose()
      labelStream .dispose()
    }

    def reset()(implicit tx: T): Unit = {
      inStream    .reset()
      gateStream  .reset()
      labelStream .reset()
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = inStream.hasNext

    def next()(implicit ctx: Context[T], tx: T): A = {
      val res = inStream.next()
      if (gateStream.hasNext && labelStream.hasNext) {
        val gateValue   = gateStream  .next()
        val labelValue  = labelStream .next()
        if (gateValue) {
          println(s"$labelValue: $res")
        }
      }
      res
    }
  }
}

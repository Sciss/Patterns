/*
 *  FormatImpl.scala
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
import de.sciss.patterns.graph.Format
import de.sciss.serial.{DataInput, DataOutput, TFormat, Writer}

import scala.collection.immutable.{IndexedSeq => Vec}

object FormatImpl extends StreamFactory {
  final val typeId = 0x466F726D // "Form"

  def expand[T <: Exec[T]](pat: Format)(implicit ctx: Context[T], tx: T): Stream[T, String] = {
    import pat._
    val sStream     = s.expand[T]
    val argStreams  = args.iterator.map(_.expand[T]).toVector
    new StreamImpl[T](sStream = sStream, argStreams = argStreams)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val sStream     = Stream.read[T, String](in)
    val argStreams  = TFormat.vec[T, Stream[T, Any]].readT(in)

    new StreamImpl[T](sStream = sStream, argStreams = argStreams)
  }

  private final class StreamImpl[T <: Exec[T]](
                                                sStream   : Stream[T, String],
                                                argStreams: Vec[Stream[T, Any]]
  )
    extends Stream[T, String] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, String] = {
      val sStreamOut     = c(sStream)
      val argStreamsOut  = argStreams.map(c(_))
      new StreamImpl[Out](sStream = sStreamOut, argStreams = argStreamsOut)
    }

    protected def typeId: Int = FormatImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      sStream.write(out)
      Writer.indexedSeq[Stream[T, Any]].write(argStreams, out)
    }

    def dispose()(implicit tx: T): Unit = {
      sStream.dispose()
      argStreams.foreach(_.dispose())
    }

    def reset()(implicit tx: T): Unit = {
      sStream.reset()
      argStreams.foreach(_.reset())
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean =
      sStream.hasNext && argStreams.forall(_.hasNext)

    def next()(implicit ctx: Context[T], tx: T): String = {
      val sVal      = sStream.next()
      val argValues = argStreams.map(_.next())
      sVal.format(argValues: _*)
    }
  }
}

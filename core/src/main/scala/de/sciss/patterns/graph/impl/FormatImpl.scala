/*
 *  FormatImpl.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package graph
package impl

import de.sciss.lucre.stm.Base
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.collection.immutable.{IndexedSeq => Vec}

object FormatImpl extends StreamFactory {
  final val typeId = 0x466F726D // "Form"

  def expand[S <: Base[S]](pat: Format)(implicit ctx: Context[S], tx: S#Tx): Stream[S, String] = {
    import pat._
    val sStream     = s.expand[S]
    val argStreams  = args.iterator.map(_.expand[S]).toVector

    new StreamImpl[S](sStream = sStream, argStreams = argStreams)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    val sStream     = Stream.read[S, String](in, access)
    val argStreams  = Serializer.indexedSeq[S#Tx, S#Acc, Stream[S, Any]].read(in, access)

    new StreamImpl[S](sStream = sStream, argStreams = argStreams)
      .asInstanceOf[Stream[S, A]] // XXX TODO --- ugly
  }

  private final class StreamImpl[S <: Base[S]](
    sStream   : Stream[S, String],
    argStreams: Vec[Stream[S, Any]]
  )
    extends Stream[S, String] {

    protected def typeId: Int = FormatImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      sStream.write(out)
      Serializer.indexedSeq[S#Tx, S#Acc, Stream[S, Any]].write(argStreams, out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      sStream.dispose()
      argStreams.foreach(_.dispose())
    }

    def reset()(implicit tx: S#Tx): Unit = {
      sStream.reset()
      argStreams.foreach(_.reset())
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
      sStream.hasNext && argStreams.forall(_.hasNext)

    def next()(implicit ctx: Context[S], tx: S#Tx): String = {
      val sVal      = sStream.next()
      val argValues = argStreams.map(_.next())
      sVal.format(argValues: _*)
    }
  }
}

/*
 *  PollImpl.scala
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
import de.sciss.serial.{DataInput, DataOutput}

object PollImpl extends StreamFactory {
  final val typeId = 0x506F6C6C // "Poll"

  def expand[S <: Base[S], A](pat: Poll[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val inStream    = in    .expand[S]
    val gateStream  = gate  .expand[S]
    val labelStream = label .expand[S]

    new StreamImpl[S, A](inStream = inStream, gateStream = gateStream, labelStream = labelStream)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    val inStream    = Stream.read[S, A      ](in, access)
    val gateStream  = Stream.read[S, Boolean](in, access)
    val labelStream = Stream.read[S, String ](in, access)

    new StreamImpl[S, A](inStream = inStream, gateStream = gateStream, labelStream = labelStream)
  }

  private final class StreamImpl[S <: Base[S], A](
    inStream    : Stream[S, A],
    gateStream  : Stream[S, Boolean],
    labelStream : Stream[S, String]
  )
    extends Stream[S, A] {

    protected def typeId: Int = PollImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      inStream    .write(out)
      gateStream  .write(out)
      labelStream .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      inStream    .dispose()
      gateStream  .dispose()
      labelStream .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = {
      inStream    .reset()
      gateStream  .reset()
      labelStream .reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = inStream.hasNext

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
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

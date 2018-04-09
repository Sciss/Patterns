/*
 *  TapImpl.scala
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
package stream

import de.sciss.lucre.stm.Base
import de.sciss.patterns
import de.sciss.patterns.graph.Tap
import de.sciss.serial.{DataInput, DataOutput}

object TapImpl extends StreamFactory {
  final val typeId = 0x54617020 // "Tap "

  def expand[S <: Base[S], A, A1](pat: Tap[A, A1])(implicit ctx: Context[S], tx: S#Tx): patterns.Stream[S, A] = {
    import pat._
    val inStream    = in  .expand[S]
    val sideStream  = side.expand[S]
    new StreamImpl[S, A, A1](inStream = inStream, sideStream = sideStream)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): patterns.Stream[S, A] = {
    val inStream    = patterns.Stream.read[S, A  ](in, access)
    val sideStream  = patterns.Stream.read[S, Any](in, access)
    new StreamImpl[S, A, Any](inStream = inStream, sideStream = sideStream)
  }

  private final class StreamImpl[S <: Base[S], A, A1](
                                                       inStream  : patterns.Stream[S, A],
                                                       sideStream: patterns.Stream[S, A1]
  )
    extends Stream[S, A] {

    protected def typeId: Int = TapImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      inStream  .write(out)
      sideStream.write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      inStream  .dispose()
      sideStream.dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = {
      inStream  .reset()
      sideStream.reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
      inStream.hasNext

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      val res = inStream.next()
      if (sideStream.hasNext) sideStream.next()
      res
    }
  }
}

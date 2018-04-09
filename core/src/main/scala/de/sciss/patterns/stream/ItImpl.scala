/*
 *  ItImpl.scala
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
import de.sciss.patterns.graph.It
import de.sciss.serial.{DataInput, DataOutput}

object ItImpl extends StreamFactory {
  final val typeId = 0x49742020 // "It  "

  def expand[S <: Base[S], A](pat: It[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val refStream = ctx.mkItStream(token)

    new StreamImpl[S, A](refStream = refStream)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val refStream  = Stream.read[S, Any](in, access)
    new StreamImpl[S, Any](refStream = refStream)
  }


  private final class StreamImpl[S <: Base[S], A](refStream: Stream[S, A])
    extends Stream[S, A] {

    protected def typeId: Int = ItImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      refStream.write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      refStream.dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = refStream.reset()

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean   = refStream.hasNext
    def next ()(implicit ctx: Context[S], tx: S#Tx): A         = refStream.next()
  }
}

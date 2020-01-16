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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.Bubble
import de.sciss.serial.{DataInput, DataOutput}

object BubbleImpl extends StreamFactory {
  final val typeId = 0x42756262 // "Bubb"

  def expand[S <: Base[S], A](pat: Bubble[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] = {
    import pat._
    val inStream = in.expand[S]
    new StreamImpl[S, A](inStream = inStream)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val inStream = Stream.read[S, Any](in, access)
    new StreamImpl[S, Any](inStream = inStream)
  }

  private final class StreamImpl[S <: Base[S], A](inStream: Stream[S, A])
    extends Stream[S, Pat[A]] {

    private[patterns] def copyStream[Out <: Base[Out]](c: Stream.Copy[S, Out])
                                                      (implicit tx: S#Tx, txOut: Out#Tx): Stream[Out, Pat[A]] = {
      val inStreamOut = c(inStream)
      new StreamImpl[Out, A](inStream = inStreamOut)
    }

    protected def typeId: Int = BubbleImpl.typeId

    protected def writeData(out: DataOutput): Unit =
      inStream.write(out)

    def dispose()(implicit tx: S#Tx): Unit =
      inStream.dispose()

    def reset()(implicit tx: S#Tx): Unit =
      inStream.reset()


    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
      inStream.hasNext

    def next()(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
      val inVal = inStream.next()
      Pat(inVal)
    }
  }
}

/*
 *  Tuple2_2Impl.scala
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
import de.sciss.patterns.graph.Tuple2_2
import de.sciss.serial.{DataInput, DataOutput}

object Tuple2_2Impl extends StreamFactory {
  final val typeId = 0x54325F32 // "T2_2"

  def expand[T <: Exec[T], A1, A](pat: Tuple2_2[A1, A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val tupStream = in.expand[T]
    new StreamImpl[T, A1, A](tupStream = tupStream)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val tupStream = Stream.read[T, (Any, Any)](in)
    new StreamImpl[T, Any, Any](tupStream = tupStream)
  }

  private final class StreamImpl[T <: Exec[T], A1, A](tupStream: Stream[T, (A1, A)])
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val tupStreamOut = c(tupStream)
      new StreamImpl[Out, A1, A](tupStream = tupStreamOut)
    }

    protected def typeId: Int = Tuple2_2Impl.typeId

    protected def writeData(out: DataOutput): Unit =
      tupStream.write(out)

    def dispose()(implicit tx: T): Unit =
      tupStream.dispose()

    def reset()(implicit tx: T): Unit = tupStream.reset()

    def hasNext(implicit ctx: Context[T], tx: T): Boolean  = tupStream.hasNext
    def next ()(implicit ctx: Context[T], tx: T): A        = tupStream.next()._2
  }
}

/*
 *  Tuple2_1Impl.scala
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
import de.sciss.patterns.graph.Tuple2_1
import de.sciss.serial.{DataInput, DataOutput}

object Tuple2_1Impl extends StreamFactory {
  final val typeId = 0x54325F31 // "T2_1"

  def expand[T <: Exec[T], A, A1](pat: Tuple2_1[A, A1])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val tupStream = in.expand[T]
    new StreamImpl[T, A, A1](tupStream = tupStream)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val tupStream = Stream.read[T, (Any, Any)](in)
    new StreamImpl[T, Any, Any](tupStream = tupStream)
  }

  private final class StreamImpl[T <: Exec[T], A, A1](tupStream: Stream[T, (A, A1)])
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val tupStreamOut = c(tupStream)
      new StreamImpl[Out, A, A1](tupStream = tupStreamOut)
    }

    protected def typeId: Int = Tuple2_1Impl.typeId

    protected def writeData(out: DataOutput): Unit =
      tupStream.write(out)

    def dispose()(implicit tx: T): Unit =
      tupStream.dispose()

    def reset()(implicit tx: T): Unit = tupStream.reset()

    def hasNext(implicit ctx: Context[T], tx: T): Boolean  = tupStream.hasNext
    def next ()(implicit ctx: Context[T], tx: T): A        = tupStream.next()._1
  }
}

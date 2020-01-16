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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.Tuple2_2
import de.sciss.serial.{DataInput, DataOutput}

object Tuple2_2Impl extends StreamFactory {
  final val typeId = 0x54325F32 // "T2_2"

  def expand[S <: Base[S], A1, A](pat: Tuple2_2[A1, A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val tupStream = in.expand[S]
    new StreamImpl[S, A1, A](tupStream = tupStream)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val tupStream = Stream.read[S, (Any, Any)](in, access)
    new StreamImpl[S, Any, Any](tupStream = tupStream)
  }

  private final class StreamImpl[S <: Base[S], A1, A](tupStream: Stream[S, (A1, A)])
    extends Stream[S, A] {

    private[patterns] def copyStream[Out <: Base[Out]](c: Stream.Copy[S, Out])
                                                      (implicit tx: S#Tx, txOut: Out#Tx): Stream[Out, A] = {
      val tupStreamOut = c(tupStream)
      new StreamImpl[Out, A1, A](tupStream = tupStreamOut)
    }

    protected def typeId: Int = Tuple2_2Impl.typeId

    protected def writeData(out: DataOutput): Unit =
      tupStream.write(out)

    def dispose()(implicit tx: S#Tx): Unit =
      tupStream.dispose()

    def reset()(implicit tx: S#Tx): Unit = tupStream.reset()

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean  = tupStream.hasNext
    def next ()(implicit ctx: Context[S], tx: S#Tx): A        = tupStream.next()._2
  }
}

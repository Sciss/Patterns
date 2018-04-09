/*
 *  Tuple2_1Impl.scala
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
import de.sciss.patterns.graph.Tuple2_1
import de.sciss.serial.{DataInput, DataOutput}

object Tuple2_1Impl extends StreamFactory {
  final val typeId = 0x54325F31 // "T2_1"

  def expand[S <: Base[S], A, A1](pat: Tuple2_1[A, A1])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val tupStream = in.expand[S]
    new StreamImpl[S, A, A1](tupStream = tupStream)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    val tupStream = Stream.read[S, (A, Any)](in, access)
    new StreamImpl[S, A, Any](tupStream = tupStream)
  }

  private final class StreamImpl[S <: Base[S], A, A1](tupStream: Stream[S, (A, A1)])
    extends Stream[S, A] {

    protected def typeId: Int = Tuple2_1Impl.typeId

    protected def writeData(out: DataOutput): Unit =
      tupStream.write(out)

    def dispose()(implicit tx: S#Tx): Unit =
      tupStream.dispose()

    def reset()(implicit tx: S#Tx): Unit = tupStream.reset()

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean  = tupStream.hasNext
    def next ()(implicit ctx: Context[S], tx: S#Tx): A        = tupStream.next()._1
  }
}

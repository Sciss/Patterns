/*
 *  Zip2Impl.scala
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
import de.sciss.patterns.graph.Zip2
import de.sciss.serial.{DataInput, DataOutput}

object Zip2Impl extends StreamFactory {
  final val typeId = 0x5A697032 // "Zip2"

  def expand[S <: Base[S], A1, A2](pat: Zip2[A1, A2])(implicit ctx: Context[S], tx: S#Tx): Stream[S, (A1, A2)] = {
    import pat._
    val aStream     = a.expand(ctx, tx)
    val bStream     = b.expand(ctx, tx)
    new StreamImpl[S, A1, A2](aStream = aStream, bStream = bStream)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val aStream     = Stream.read[S, Any](in, access)
    val bStream     = Stream.read[S, Any](in, access)

    new StreamImpl[S, Any, Any](aStream = aStream, bStream = bStream)
  }


  private final class StreamImpl[S <: Base[S], A1, A2](
                                                        val aStream: Stream[S, A1],
                                                        val bStream: Stream[S, A2]
  )
    extends Stream[S, (A1, A2)] {

    private[patterns] def copyStream[Out <: Base[Out]]()(implicit tx: S#Tx, txOut: Out#Tx,
                                                         ctx: Context[Out]): Stream[Out, (A1, A2)] = {
      val aStreamOut = aStream.copyStream[Out]()
      val bStreamOut = bStream.copyStream[Out]()
      new StreamImpl[Out, A1, A2](aStream = aStreamOut, bStream = bStreamOut)
    }

    protected def typeId: Int = Zip2Impl.typeId

    protected def writeData(out: DataOutput): Unit = {
      aStream.write(out)
      bStream.write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      aStream.dispose()
      bStream.dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = {
      aStream.reset()
      bStream.reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = aStream.hasNext && bStream.hasNext

    def next()(implicit ctx: Context[S], tx: S#Tx): (A1, A2) = (aStream.next(), bStream.next())
  }
}
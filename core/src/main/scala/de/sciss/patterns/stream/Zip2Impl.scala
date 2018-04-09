/*
 *  Zip2Impl.scala
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
import de.sciss.patterns.graph.Zip2
import de.sciss.serial.{DataInput, DataOutput}

object Zip2Impl extends StreamFactory {
  final val typeId = 0x5A697032 // "Zip2"

  def expand[S <: Base[S], A1, A2](pat: Zip2[A1, A2])(implicit ctx: Context[S], tx: S#Tx): patterns.Stream[S, (A1, A2)] = {
    import pat._
    val aStream     = a.expand(ctx, tx)
    val bStream     = b.expand(ctx, tx)

    new StreamImpl[S, A1, A2](aStream = aStream, bStream = bStream)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): patterns.Stream[S, A] = {
    val aStream     = patterns.Stream.read[S, Any](in, access)
    val bStream     = patterns.Stream.read[S, Any](in, access)

    new StreamImpl[S, Any, Any](aStream = aStream, bStream = bStream)
      .asInstanceOf[patterns.Stream[S, A]] // XXX TODO --- ugly
  }


  private final class StreamImpl[S <: Base[S], A1, A2](
                                                        val aStream: patterns.Stream[S, A1],
                                                        val bStream: patterns.Stream[S, A2]
  )
    extends Stream[S, (A1, A2)] {

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
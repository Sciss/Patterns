/*
 *  CatImpl.scala
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
import de.sciss.patterns.Types.{Aux, Widen2}
import de.sciss.patterns.graph.Cat
import de.sciss.serial.{DataInput, DataOutput}

object CatImpl extends StreamFactory {
  final val typeId = 0x43617420 // "Cat "

  def expand[S <: Base[S], A1, A2, A](pat: Cat[A1, A2, A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val aStream     = a.expand[S]
    val bStream     = b.expand[S]

    new StreamImpl[S, A1, A2, A](aStream = aStream, bStream = bStream, widen = widen)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    val aStream     = Stream.read[S, Any](in, access)
    val bStream     = Stream.read[S, Any](in, access)
    val widen       = Aux.readT[Widen2[Any, Any, A]](in)

    new StreamImpl[S, Any, Any, A](aStream = aStream, bStream = bStream, widen = widen)
  }

  private final class StreamImpl[S <: Base[S], A1, A2, A](
                                                           aStream   : Stream[S, A1],
                                                           bStream   : Stream[S, A2],
                                                           widen     : Widen2[A1, A2, A]
  )
    extends Stream[S, A] {

    protected def typeId: Int = CatImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      aStream.write(out)
      bStream.write(out)
      widen  .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      aStream.dispose()
      bStream.dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = {
      aStream.reset()
      bStream.reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      val res = aStream.hasNext || bStream.hasNext
      //      logStream(s"Cat.iterator.hasNext = $res")
      res
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      val ahn = aStream.hasNext
      val res = if (ahn) {
        val aVal = aStream.next()
        widen.widen1(aVal)
      } else {
        val bVal = bStream.next()
        widen.widen2(bVal)
      }
      logStream(s"Cat.iterator.next(); ai.hasNext = $ahn; res = $res") // ${stream.hashCode().toHexString}
      res
    }
  }
}

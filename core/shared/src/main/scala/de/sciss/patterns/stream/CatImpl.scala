/*
 *  CatImpl.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package stream

import de.sciss.lucre.Adjunct
import de.sciss.lucre.Adjunct.Widen2
import de.sciss.lucre.Exec
import de.sciss.patterns.graph.Cat
import de.sciss.serial.{DataInput, DataOutput}
import de.sciss.patterns.Log.{stream => logStream}

object CatImpl extends StreamFactory {
  final val typeId = 0x43617420 // "Cat "

  def expand[T <: Exec[T], A1, A2, A](pat: Cat[A1, A2, A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val aStream     = a.expand[T]
    val bStream     = b.expand[T]
    new StreamImpl[T, A1, A2, A](aStream = aStream, bStream = bStream, widen = widen)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val aStream     = Stream.read[T, Any](in)
    val bStream     = Stream.read[T, Any](in)
    val widen       = Adjunct.readT[Widen2[Any, Any, Any]](in)

    new StreamImpl[T, Any, Any, Any](aStream = aStream, bStream = bStream, widen = widen)
  }

  private final class StreamImpl[T <: Exec[T], A1, A2, A](
                                                           aStream   : Stream[T, A1],
                                                           bStream   : Stream[T, A2],
                                                           widen     : Widen2[A1, A2, A]
  )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val aStreamOut  = c(aStream)
      val bStreamOut  = c(bStream)
      new StreamImpl[Out, A1, A2, A](aStream = aStreamOut, bStream = bStreamOut, widen = widen)
    }

    protected def typeId: Int = CatImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      aStream.write(out)
      bStream.write(out)
      widen  .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      aStream.dispose()
      bStream.dispose()
    }

    def reset()(implicit tx: T): Unit = {
      aStream.reset()
      bStream.reset()
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      val res = aStream.hasNext || bStream.hasNext
      //      logStream(s"Cat.iterator.hasNext = $res")
      res
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
      val ahn = aStream.hasNext
      val res = if (ahn) {
        val aVal = aStream.next()
        widen.widen1(aVal)
      } else {
        val bVal = bStream.next()
        widen.widen2(bVal)
      }
      // $COVERAGE-OFF$
      logStream.debug(s"Cat.iterator.next(); ai.hasNext = $ahn; res = $res") // ${stream.hashCode().toHexString}
      // $COVERAGE-ON$
      res
    }
  }
}

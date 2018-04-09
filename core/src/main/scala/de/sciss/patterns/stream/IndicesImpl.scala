/*
 *  IndicesImpl.scala
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
import de.sciss.patterns.graph.Indices
import de.sciss.serial.{DataInput, DataOutput}

object IndicesImpl extends StreamFactory {
  final val typeId = 0x496E6469 // "Indi"

  def expand[S <: Base[S], A](pat: Indices[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, Int] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in.expand[S]
    val count     = tx.newIntVar(id, 0)

    new StreamImpl[S, A](id = id, inStream = inStream, count = count)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    val id        = tx.newId()
    val inStream  = Stream.read[S, A](in, access)
    val count     = tx.newIntVar(id, 0)

    new StreamImpl[S, A](id = id, inStream = inStream, count = count)
      .asInstanceOf[Stream[S, A]] // XXX TODO --- ugly
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   id      : S#Id,
                                                   inStream: Stream[S, A],
                                                   count   : S#Var[Int]
  )
    extends Stream[S, Int] {

    protected def typeId: Int = IndicesImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      inStream.write(out)
      count   .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id      .dispose()
      inStream.dispose()
      count   .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = {
      inStream.reset()
      count() = 0
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
      inStream.hasNext

    def next()(implicit ctx: Context[S], tx: S#Tx): Int = {
      val res = count()
      inStream.next()
      count() = res + 1
      res
    }
  }
}

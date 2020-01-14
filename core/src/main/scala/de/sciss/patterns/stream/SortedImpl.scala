/*
 *  SortedImpl.scala
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

import de.sciss.lucre.adjunct.Adjunct
import de.sciss.lucre.adjunct.Adjunct.ScalarOrd
import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.Sorted
import de.sciss.serial.{DataInput, DataOutput}

object SortedImpl extends StreamFactory {
  final val typeId = 0x536F7274 // "Sort"

  def expand[S <: Base[S], A](pat: Sorted[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id            = tx.newId()
    val inStream      = in.expand(ctx, tx)
    val sortedStream  = tx.newVar[Stream[S, A]](id, null)
    val valid         = tx.newBooleanVar(id, false)

    new StreamImpl[S, A](id = id, inStream = inStream, sortedStream = sortedStream, valid = valid)(ord)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id            = tx.readId(in, access)
    val inStream      = Stream.read[S, Any](in, access)
    val sortedStream  = tx.readVar[Stream[S, Any]](id, in)
    val valid         = tx.readBooleanVar(id, in)
    val ord           = Adjunct.readT[ScalarOrd[Any]](in)

    new StreamImpl[S, Any](id = id, inStream = inStream, sortedStream = sortedStream, valid = valid)(ord)
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   id          : S#Id,
                                                   inStream    : Stream[S, A],
                                                   sortedStream: S#Var[Stream[S, A]],
                                                   valid       : S#Var[Boolean]
  )(
    implicit ord: ScalarOrd[A]
  )
    extends Stream[S, A] {
    
    protected def typeId: Int = SortedImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id          .write(out)
      inStream    .write(out)
      sortedStream.write(out)
      valid       .write(out)
      ord         .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id          .dispose()
      inStream    .dispose()
      sortedStream.dispose()
      valid       .dispose()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      val xs          = inStream.toList
      sortedStream()  = Stream(xs.sortWith(ord.lt): _*)
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      inStream.reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      sortedStream().hasNext
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      validate()
      sortedStream().next()
    }
  }
}

/*
 *  SortedImpl.scala
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

import de.sciss.lucre.{Adjunct, Exec, Ident, Var}
import de.sciss.lucre.Adjunct.ScalarOrd
import de.sciss.patterns.graph.Sorted
import de.sciss.serial.{DataInput, DataOutput}

object SortedImpl extends StreamFactory {
  final val typeId = 0x536F7274 // "Sort"

  def expand[T <: Exec[T], A](pat: Sorted[A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val id            = tx.newId()
    val inStream      = in.expand(ctx, tx)
    val sortedStream  = id.newVar[Stream[T, A]](null)
    val valid         = id.newBooleanVar(false)
    new StreamImpl[T, A](id = id, inStream = inStream, sortedStream = sortedStream, valid = valid)(ord)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id            = tx.readId(in)
    val inStream      = Stream.read[T, Any](in)
    val sortedStream  = id.readVar[Stream[T, Any]](in)
    val valid         = id.readBooleanVar(in)
    val ord           = Adjunct.readT[ScalarOrd[Any]](in)

    new StreamImpl[T, Any](id = id, inStream = inStream, sortedStream = sortedStream, valid = valid)(ord)
  }

  private final class StreamImpl[T <: Exec[T], A](
                                                   id          : Ident[T],
                                                   inStream    : Stream[T, A],
                                                   sortedStream: Var[T, Stream[T, A]],
                                                   valid       : Var[T, Boolean]
  )(
    implicit ord: ScalarOrd[A]
  )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut           = txOut.newId()
      val inStreamOut     = c(inStream)
      val sortedStreamOut = c.copyVar(idOut, sortedStream)
      val validOut        = idOut.newBooleanVar(valid())
      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, sortedStream = sortedStreamOut, valid = validOut)(ord)
    }

    protected def typeId: Int = SortedImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id          .write(out)
      inStream    .write(out)
      sortedStream.write(out)
      valid       .write(out)
      ord         .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id          .dispose()
      inStream    .dispose()
      sortedStream.dispose()
      valid       .dispose()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      val xs          = inStream.toList
      sortedStream()  = Stream(xs.sortWith(ord.lt): _*)
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      inStream.reset()
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      sortedStream().hasNext
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
      validate()
      sortedStream().next()
    }
  }
}

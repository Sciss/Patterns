/*
 *  PatSeqImpl.scala
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

import de.sciss.lucre.{Exec, Ident, Var}
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec}

object PatSeqImpl extends StreamFactory {
  final val typeId = 0x53657120 // "Seq "

  def apply[T <: Exec[T], A](elem: Seq[A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    val id    = tx.newId()
    val xs    = elem.toIndexedSeq
    val count = id.newIntVar(0)
    new StreamImpl[T, A](id = id, xs = xs, count = count)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id    = tx.readId(in)
    val xs    = PatElem.vecFormat[Any].read(in)
    val count = id.readIntVar(in)
    new StreamImpl[T, Any](id = id, xs = xs, count = count)
  }

  private final class StreamImpl[T <: Exec[T], A](
    id    : Ident[T],
    xs    : Vec[A],
    count : Var[T, Int]
  )
    extends Stream[T, A] {

    private[patterns] override def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                               (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut     = txOut.newId()
      val countOut  = idOut.newIntVar(count())
      new StreamImpl[Out, A](id = idOut, xs = xs, count = countOut)
    }

    protected def typeId: Int = PatSeqImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id.write(out)
      PatElem.vecFormat[A].write(xs, out)
      count.write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id    .dispose()
      count .dispose()
    }

    private[this] lazy val simpleString =
      xs.mkString("Stream(", ", ", ")")

    override def toString = s"$simpleString; count = $count"

    def reset()(implicit tx: T): Unit =
      count() = 0

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = count() < xs.size

    def next ()(implicit ctx: Context[T], tx: T): A = {
      if (!hasNext) Stream.exhausted()
      val i = count()
      count() = i + 1
      val res = xs(i)
      // logStream(s"$simpleString.next(); count = $i; res = $res")
      res
    }
  }
}

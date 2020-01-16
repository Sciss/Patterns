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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec}

object PatSeqImpl extends StreamFactory {
  final val typeId = 0x53657120 // "Seq "

  def apply[S <: Base[S], A](elem: Seq[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    val id    = tx.newId()
    val xs    = elem.toIndexedSeq
    val count = tx.newIntVar(id, 0)
    new StreamImpl[S, A](id = id, xs = xs, count = count)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id    = tx.readId(in, access)
    val xs    = PatElem.vecSerializer[Any].read(in)
    val count = tx.readIntVar(id, in)
    new StreamImpl[S, Any](id = id, xs = xs, count = count)
  }

  private final class StreamImpl[S <: Base[S], A](
    id    : S#Id,
    xs    : Vec[A],
    count : S#Var[Int]
  )
    extends Stream[S, A] {

    private[patterns] override def copyStream[Out <: Base[Out]](c: Stream.Copy[S, Out])
                                                               (implicit tx: S#Tx, txOut: Out#Tx): Stream[Out, A] = {
      val idOut     = txOut.newId()
      val countOut  = txOut.newIntVar(idOut, count())
      new StreamImpl[Out, A](id = idOut, xs = xs, count = countOut)
    }

    protected def typeId: Int = PatSeqImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id.write(out)
      PatElem.vecSerializer[A].write(xs, out)
      count.write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id    .dispose()
      count .dispose()
    }

    private[this] lazy val simpleString =
      xs.mkString("Stream(", ", ", ")")

    override def toString = s"$simpleString; count = $count"

    def reset()(implicit tx: S#Tx): Unit =
      count() = 0

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = count() < xs.size

    def next ()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val i = count()
      count() = i + 1
      val res = xs(i)
      // logStream(s"$simpleString.next(); count = $i; res = $res")
      res
    }
  }
}

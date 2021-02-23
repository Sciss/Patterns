/*
 *  ConstantImpl.scala
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

import de.sciss.lucre.Exec
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

object ConstantImpl extends StreamFactory {
  final val typeId = 0x436F6E73 // "Cons"

  def apply[T <: Exec[T], A](elem: A): Stream[T, A] =
    new StreamImpl[T, A](elem)

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val elem = PatElem.read[Any](in)
    new StreamImpl[T, Any](elem)
  }

  private final class StreamImpl[T <: Exec[T], A](elem: A) extends Stream[T, A] {
    override def toString = s"Stream.constant($elem)@${hashCode().toHexString}"

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val elemOut = elem
      new StreamImpl[Out, A](elemOut) // we could even write `this.asInstanceOf[Stream[Out, A]]` but it's ugly
    }

    protected def typeId: Int = ConstantImpl.typeId

    protected def writeData(out: DataOutput): Unit =
      PatElem.write(elem, out)

    def dispose()(implicit tx: T): Unit = ()

    def reset()(implicit tx: T): Unit = ()

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = true
    def next ()(implicit ctx: Context[T], tx: T): A       = elem
  }
}

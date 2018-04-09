/*
 *  ConstantImpl.scala
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
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

object ConstantImpl extends StreamFactory {
  final val typeId = 0x436F6E73 // "Cons"

  def apply[S <: Base[S], A](elem: A): Stream[S, A] =
    new StreamImpl[S, A](elem)

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    val elem = PatElem.read[A](in)
    new StreamImpl[S, A](elem)
  }

  private final class StreamImpl[S <: Base[S], A](elem: A) extends Stream[S, A] {
    override def toString = s"Stream.constant($elem)@${hashCode().toHexString}"

    protected def typeId: Int = ConstantImpl.typeId

    protected def writeData(out: DataOutput): Unit =
      PatElem.write(elem, out)

    def dispose()(implicit tx: S#Tx): Unit = ()

    def reset()(implicit tx: S#Tx): Unit = ()

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = true
    def next ()(implicit ctx: Context[S], tx: S#Tx): A       = elem
  }
}

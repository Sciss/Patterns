/*
 *  EmptyImpl.scala
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
import de.sciss.serial.{DataInput, DataOutput}

object EmptyImpl extends StreamFactory {
  final val typeId = 0x456D7074 // "Empt"

  def apply[T <: Exec[T]]()(implicit tx: T): Stream[T, Nothing] =
    new StreamImpl[T]

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] =
    new StreamImpl[T]

  private final class StreamImpl[T <: Exec[T]]
    extends Stream[T, Nothing] {

    protected def typeId: Int = EmptyImpl.typeId

    protected def writeData(out: DataOutput): Unit = ()

    def dispose()(implicit tx: T): Unit = ()

    def reset()(implicit tx: T): Unit = ()

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = false

    def next()(implicit ctx: Context[T], tx: T): Nothing =
      Stream.exhausted()

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, Nothing] =
      EmptyImpl[Out]()
  }
}

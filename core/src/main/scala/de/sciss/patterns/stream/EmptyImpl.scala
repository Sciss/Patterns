/*
 *  EmptyImpl.scala
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
import de.sciss.serial.{DataInput, DataOutput}

object EmptyImpl extends StreamFactory {
  final val typeId = 0x456D7074 // "Empt"

  def apply[S <: Base[S]]()(implicit tx: S#Tx): Stream[S, Nothing] =
    new StreamImpl[S]

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] =
    new StreamImpl[S]

  private final class StreamImpl[S <: Base[S]]
    extends Stream[S, Nothing] {

    protected def typeId: Int = EmptyImpl.typeId

    protected def writeData(out: DataOutput): Unit = ()

    def dispose()(implicit tx: S#Tx): Unit = ()

    def reset()(implicit tx: S#Tx): Unit = ()

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = false

    def next()(implicit ctx: Context[S], tx: S#Tx): Nothing =
      Stream.exhausted()

    private[patterns] def copyStream[Out <: Base[Out]](c: Stream.Copy[S, Out])
                                                      (implicit tx: S#Tx, txOut: Out#Tx): Stream[Out, Nothing] =
      EmptyImpl[Out]()
  }
}

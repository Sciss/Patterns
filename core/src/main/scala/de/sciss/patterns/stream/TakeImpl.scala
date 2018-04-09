/*
 *  TakeImpl.scala
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
import de.sciss.patterns.graph.Take
import de.sciss.patterns.stream.impl.TruncateLikeStreamImpl
import de.sciss.serial.{DataInput, DataOutput}

object TakeImpl extends StreamFactory {
  final val typeId = 0x54616B65 // "Take"

  def expand[S <: Base[S], A](pat: Take[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in    .expand[S]
    val lenStream = length.expand[S]
    val _hasNext  = tx.newBooleanVar(id, false)
    val valid     = tx.newBooleanVar(id, false)
    val remain    = tx.newIntVar    (id, 0)

    new StreamImpl[S, A](id = id, inStream = inStream, lenStream = lenStream, _hasNext = _hasNext, valid = valid,
      remain = remain)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id        = tx.readId(in, access)
    val inStream  = Stream.read[S, Any  ](in, access)
    val lenStream = Stream.read[S, Int](in, access)
    val _hasNext  = tx.readBooleanVar(id, in)
    val valid     = tx.readBooleanVar(id, in)
    val remain    = tx.readIntVar    (id, in)

    new StreamImpl[S, Any](id = id, inStream = inStream, lenStream = lenStream, _hasNext = _hasNext, valid = valid,
      remain = remain)
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   protected val id        : S#Id,
                                                   protected val inStream  : Stream[S, A],
                                                   protected val lenStream : Stream[S, Int],
                                                   protected val _hasNext  : S#Var[Boolean],
                                                   protected val valid     : S#Var[Boolean],
                                                   protected val remain    : S#Var[Int]
                                                 )
    extends TruncateLikeStreamImpl[S, A] {

    protected def typeId: Int = TakeImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id        .write(out)
      inStream  .write(out)
      lenStream .write(out)
      _hasNext  .write(out)
      valid     .write(out)
      remain    .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id        .dispose()
      inStream  .dispose()
      lenStream .dispose()
      _hasNext  .dispose()
      valid     .dispose()
      remain    .dispose()
    }

    protected def validateWithLen(n: Int)(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      remain() = n
      n > 0 && inStream.hasNext
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = inStream.next()
      val n1 = remain() - 1
      remain() = n1
      if (n1 == 0 || !inStream.hasNext) _hasNext() = false
      res
    }
  }
}

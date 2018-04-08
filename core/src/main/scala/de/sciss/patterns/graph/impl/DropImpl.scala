/*
 *  DropImpl.scala
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
package graph
package impl

import de.sciss.lucre.stm.Base
import de.sciss.serial.{DataInput, DataOutput}

object DropImpl extends StreamFactory {
  final val typeId = 0x44726F70 // "Drop

  def expand[S <: Base[S], A](pat: Drop[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in    .expand[S]
    val lenStream = length.expand[S]
    val _hasNext  = tx.newBooleanVar(id, false)
    val valid     = tx.newBooleanVar(id, false)

    new StreamImpl[S, A](id = id, inStream = inStream, lenStream = lenStream, _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    val id        = tx.readId(in, access)
    val inStream  = Stream.read[S, A  ](in, access)
    val lenStream = Stream.read[S, Int](in, access)
    val _hasNext  = tx.readBooleanVar(id, in)
    val valid     = tx.readBooleanVar(id, in)

    new StreamImpl[S, A](id = id, inStream = inStream, lenStream = lenStream, _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[S <: Base[S], A](
    protected val id        : S#Id,
    protected val inStream  : Stream[S, A],
    protected val lenStream : Stream[S, Int],
    protected val _hasNext  : S#Var[Boolean],
    protected val valid     : S#Var[Boolean]
  )
    extends TruncateStream[S, A] {

    protected def typeId: Int = DropImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id        .write(out)
      inStream  .write(out)
      lenStream .write(out)
      _hasNext  .write(out)
      valid     .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id        .dispose()
      inStream  .dispose()
      lenStream .dispose()
      _hasNext  .dispose()
      valid     .dispose()
    }

    protected def validateWithLen(n: Int)(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      var i = 0
      while (i < n && inStream.hasNext) {
        inStream.next()
        i += 1
      }
      inStream.hasNext
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = inStream.next()
      if (!inStream.hasNext) _hasNext() = false
      res
    }
  }
}

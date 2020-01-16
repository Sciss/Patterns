/*
 *  DropImpl.scala
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
import de.sciss.patterns.graph.Drop
import de.sciss.patterns.stream.impl.TruncateLikeStreamImpl
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

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id        = tx.readId(in, access)
    val inStream  = Stream.read[S, Any](in, access)
    val lenStream = Stream.read[S, Int](in, access)
    val _hasNext  = tx.readBooleanVar(id, in)
    val valid     = tx.readBooleanVar(id, in)

    new StreamImpl[S, Any](id = id, inStream = inStream, lenStream = lenStream, _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   protected val id        : S#Id,
                                                   protected val inStream  : Stream[S, A],
                                                   protected val lenStream : Stream[S, Int],
                                                   protected val _hasNext  : S#Var[Boolean],
                                                   protected val valid     : S#Var[Boolean]
  )
    extends TruncateLikeStreamImpl[S, A] {

    private[patterns] def copyStream[Out <: Base[Out]](c: Stream.Copy[S, Out])
                                                      (implicit tx: S#Tx, txOut: Out#Tx): Stream[Out, A] = {
      val idOut         = txOut.newId()
      val inStreamOut   = c(inStream )
      val lenStreamOut  = c(lenStream)
      val hasNextOut    = txOut.newBooleanVar(idOut, _hasNext())
      val validOut      = txOut.newBooleanVar(idOut, valid())

      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, lenStream = lenStreamOut,
        _hasNext = hasNextOut, valid = validOut)
    }

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

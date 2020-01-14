/*
 *  LengthImpl.scala
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
import de.sciss.patterns.graph.Length
import de.sciss.serial.{DataInput, DataOutput}

object LengthImpl extends StreamFactory {
  final val typeId = 0x4C656E67 // "Leng"

  def expand[S <: Base[S], A](pat: Length[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, Int] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in.expand[S]
    val _hasNext  = tx.newBooleanVar(id, true)

    new StreamImpl[S, A](id = id, inStream = inStream, _hasNext = _hasNext)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id        = tx.readId(in, access)
    val inStream  = Stream.read[S, Any](in, access)
    val _hasNext  = tx.readBooleanVar(id, in)

    new StreamImpl[S, Any](id = id, inStream = inStream, _hasNext = _hasNext)
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   id      : S#Id,
                                                   inStream: Stream[S, A],
                                                   _hasNext: S#Var[Boolean]
  )
    extends Stream[S, Int] {

    protected def typeId: Int = LengthImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      inStream.write(out)
      _hasNext.write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id      .dispose()
      inStream.dispose()
      _hasNext.dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = {
      inStream.reset()
      _hasNext() = true
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = _hasNext()

    def next()(implicit ctx: Context[S], tx: S#Tx): Int = {
      if (!hasNext) Stream.exhausted()
      var res = 0
      while (inStream.hasNext) {
        inStream.next()
        res += 1
      }
      _hasNext() = false
      res
    }
  }

}

/*
 *  IndexOfSliceImpl.scala
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
import de.sciss.patterns.graph.IndexOfSlice
import de.sciss.serial.{DataInput, DataOutput}

import scala.collection.mutable

object IndexOfSliceImpl extends StreamFactory {
  final val typeId = 0x4978536C // "IxSl"

  def expand[S <: Base[S], A1, A2](pat: IndexOfSlice[A1, A2])(implicit ctx: Context[S], tx: S#Tx): Stream[S, Int] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in  .expand[S]
    val subStream   = sub .expand[S]
    val fromStream  = from.expand[S]
    val fromValue   = tx.newIntVar    (id, 0    )
    val _hasNext    = tx.newBooleanVar(id, false)
    val valid       = tx.newBooleanVar(id, false)

    new StreamImpl[S, A1, A2](id = id, inStream = inStream, subStream = subStream, fromStream = fromStream,
      fromValue = fromValue, _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id          = tx.readId(in, access)
    val inStream    = Stream.read[S, Any](in, access)
    val subStream   = Stream.read[S, Any](in, access)
    val fromStream  = Stream.read[S, Int](in, access)
    val fromValue   = tx.readIntVar    (id, in)
    val _hasNext    = tx.readBooleanVar(id, in)
    val valid       = tx.readBooleanVar(id, in)

    new StreamImpl[S, Any, Any](id = id, inStream = inStream, subStream = subStream, fromStream = fromStream,
      fromValue = fromValue, _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[S <: Base[S], A1, A2](
                                                        id        : S#Id,
                                                        inStream  : Stream[S, A1],
                                                        subStream : Stream[S, A2],
                                                        fromStream: Stream[S, Int],
                                                        fromValue : S#Var[Int],
                                                        _hasNext  : S#Var[Boolean],
                                                        valid     : S#Var[Boolean]
  )
    extends Stream[S, Int] {

    protected def typeId: Int = IndexOfSliceImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id        .write(out)
      inStream  .write(out)
      subStream .write(out)
      fromStream.write(out)
      fromValue .write(out)
      _hasNext  .write(out)
      valid     .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id        .dispose()
      inStream  .dispose()
      subStream .dispose()
      fromStream.dispose()
      fromValue .dispose()
      _hasNext  .dispose()
      valid     .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      inStream  .reset()
      subStream .reset()
      fromStream.reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      val hn = fromStream.hasNext
      _hasNext() = hn
      if (hn) {
        val _fromValue = math.max(0, fromStream.next())
        fromValue() = _fromValue
      }
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): Int = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      _hasNext()      = false   // there is only one run through

      val subVec      = subStream.toVector
      val subSz       = subVec.size
      val _fromValue  = fromValue()
      var idx = 0
      while (idx < _fromValue && inStream.hasNext) {
        inStream.next()
        idx += 1
      }
      if (idx < _fromValue) return -1

      val q   = mutable.Queue.empty[A1]
      var qSz = 0
      while (qSz < subSz && inStream.hasNext) {
        val c = inStream.next()
        q.enqueue(c)
        qSz += 1
      }

      if (qSz < subSz) return -1
      assert(q.size == subSz)

      while (q != subVec) {
        if (!inStream.hasNext) return -1
        val c = inStream.next()
        q.dequeue()
        q.enqueue(c)
        idx += 1
      }

      idx
    }
  }
}

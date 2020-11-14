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

import de.sciss.lucre.{Exec, Ident, Var}
import de.sciss.patterns.graph.IndexOfSlice
import de.sciss.serial.{DataInput, DataOutput}

import scala.collection.mutable

object IndexOfSliceImpl extends StreamFactory {
  final val typeId = 0x4978536C // "IxSl"

  def expand[T <: Exec[T], A1, A2](pat: IndexOfSlice[A1, A2])(implicit ctx: Context[T], tx: T): Stream[T, Int] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in  .expand[T]
    val subStream   = sub .expand[T]
    val fromStream  = from.expand[T]
    val fromValue   = id.newIntVar(0)
    val _hasNext    = id.newBooleanVar(false)
    val valid       = id.newBooleanVar(false)

    new StreamImpl[T, A1, A2](id = id, inStream = inStream, subStream = subStream, fromStream = fromStream,
      fromValue = fromValue, _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id          = tx.readId(in)
    val inStream    = Stream.read[T, Any](in)
    val subStream   = Stream.read[T, Any](in)
    val fromStream  = Stream.read[T, Int](in)
    val fromValue   = id.readIntVar(in)
    val _hasNext    = id.readBooleanVar(in)
    val valid       = id.readBooleanVar(in)

    new StreamImpl[T, Any, Any](id = id, inStream = inStream, subStream = subStream, fromStream = fromStream,
      fromValue = fromValue, _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[T <: Exec[T], A1, A2](
                                                        id        : Ident[T],
                                                        inStream  : Stream[T, A1],
                                                        subStream : Stream[T, A2],
                                                        fromStream: Stream[T, Int],
                                                        fromValue : Var[T, Int],
                                                        _hasNext  : Var[T, Boolean],
                                                        valid     : Var[T, Boolean]
  )
    extends Stream[T, Int] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, Int] = {
      val idOut         = txOut.newId()
      val inStreamOut   = c(inStream  )
      val subStreamOut  = c(subStream )
      val fromStreamOut = c(fromStream)
      val fromValueOut  = idOut.newIntVar(fromValue())
      val hasNextOut    = idOut.newBooleanVar(_hasNext())
      val validOut      = idOut.newBooleanVar(valid())

      new StreamImpl[Out, A1, A2](id = idOut, inStream = inStreamOut, subStream = subStreamOut, fromStream = fromStreamOut,
        fromValue = fromValueOut, _hasNext = hasNextOut, valid = validOut)
    }

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

    def dispose()(implicit tx: T): Unit = {
      id        .dispose()
      inStream  .dispose()
      subStream .dispose()
      fromStream.dispose()
      fromValue .dispose()
      _hasNext  .dispose()
      valid     .dispose()
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      inStream  .reset()
      subStream .reset()
      fromStream.reset()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      val hn = fromStream.hasNext
      _hasNext() = hn
      if (hn) {
        val _fromValue = math.max(0, fromStream.next())
        fromValue() = _fromValue
      }
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[T], tx: T): Int = {
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

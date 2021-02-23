/*
 *  ParImpl.scala
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

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.{Exec, Ident, Var}
import de.sciss.patterns.graph.{Par, Pat}
import de.sciss.patterns.impl.TimeRef
import de.sciss.serial.{DataInput, DataOutput}

object ParImpl extends StreamFactory {
  final val typeId = 0x50617220 // "Par "

  def expand[T <: Exec[T]](pat: Par)(implicit ctx: Context[T], tx: T): Stream[T, Event] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in.expand[T]
    val pq          = SkipList.Map.empty[T, TimeRef, Stream[T, Event]]
    val elem        = id.newVar[Event](Event.empty)
    val _hasNext    = id.newBooleanVar(false)
    val valid       = id.newBooleanVar(false)

    new StreamImpl[T](id = id, inStream = inStream, pq = pq, elem = elem, _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id          = tx.readId(in)
    val inStream    = Stream.read[T, Pat[Event]](in)
    val pq          = SkipList.Map.read[T, TimeRef, Stream[T, Event]](in)
    val elem        = id.readVar[Event](in)
    val _hasNext    = id.readBooleanVar(in)
    val valid       = id.readBooleanVar(in)

    new StreamImpl[T](id = id, inStream = inStream, pq = pq, elem = elem, _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[T <: Exec[T]](
                                               id       : Ident[T],
                                               inStream : Stream[T, Pat[Event]],
                                               pq       : SkipList.Map[T, TimeRef, Stream[T, Event]],
                                               elem     : Var[T, Event],
                                               _hasNext : Var[T, Boolean],
                                               valid    : Var[T, Boolean]
                                              ) extends Stream[T, Event] {
    protected def typeId: Int = ParImpl.typeId

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, Event] = {
      val idOut       = txOut.newId()
      val inStreamOut = c(inStream)
      import c.context
      val pqOut       = {
        val sl = SkipList.Map.empty[Out, TimeRef, Stream[Out, Event]]
        pq.iterator.foreach {
          case (key, value) =>
            sl.put(key, c(value))
        }
        sl
      }
      val elemOut     = idOut.newVar[Event](elem())
      val hasNextOut  = idOut.newBooleanVar(_hasNext())
      val validOut    = idOut.newBooleanVar(valid())

      new StreamImpl[Out](id = idOut, inStream = inStreamOut, pq = pqOut, elem = elemOut, _hasNext = hasNextOut, valid = validOut)
    }

    protected def writeData(out: DataOutput): Unit = {
      id       .write(out)
      inStream .write(out)
      pq       .write(out)
      elem     .write(out)
      _hasNext .write(out)
      valid    .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id       .dispose()
      inStream .dispose()
      pq       .dispose()
      elem     .dispose()
      _hasNext .dispose()
      valid    .dispose()
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      inStream.reset()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      pq.clear()
      var refCnt = 0
      val _in = inStream
      while (_in.hasNext) {
        val pat = _in.next()
        val it = pat.expand[T]
        if (it.hasNext) {
          pq.put(new TimeRef(refCnt), it)
          refCnt += 1
        }
      }

      advance()
    }

    private def advance()(implicit ctx: Context[T], tx: T): Unit =
      if (pq.nonEmpty) {
        val (ref, it) = pq.head
        pq.remove(ref)
        val _elem = it.next()

        elem()    = _elem
        val d     = math.max(0.0, Event.delta(_elem))
        val now   = ref.time
        if (it.hasNext) {
          val refNew = ref.advance(d)
          pq.put(refNew, it)
        }
        if (pq.nonEmpty) {
          val nextTime = pq.firstKey.time
          elem() = elem() + (Event.keyDelta -> (nextTime - now))
        }
        _hasNext() = true

      } else {
        _hasNext() = false
      }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[T], tx: T): Event = {
      if (!hasNext) Stream.exhausted()
      val res = elem()
      advance()
      res
    }
  }
}

/*
 *  ParImpl.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.Par
import de.sciss.patterns.impl.TimeRef
import de.sciss.serial.{DataInput, DataOutput}

object ParImpl extends StreamFactory {
  final val typeId = 0x50617220 // "Par "

  def expand[S <: Base[S]](pat: Par)(implicit ctx: Context[S], tx: S#Tx): Stream[S, Event] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in.expand[S]
    val pq          = SkipList.Map.empty[S, TimeRef, Stream[S, Event]]
    val elem        = tx.newVar[Event](id, Event.empty)
    val _hasNext    = tx.newBooleanVar(id, false)
    val valid       = tx.newBooleanVar(id, false)

    new StreamImpl[S](id = id, inStream = inStream, pq = pq, elem = elem, _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id          = tx.readId(in, access)
    val inStream    = Stream.read[S, Pat[Event]](in, access)
    val pq          = SkipList.Map.read[S, TimeRef, Stream[S, Event]](in, access)
    val elem        = tx.readVar[Event](id, in)
    val _hasNext    = tx.readBooleanVar(id, in)
    val valid       = tx.readBooleanVar(id, in)

    new StreamImpl[S](id = id, inStream = inStream, pq = pq, elem = elem, _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[S <: Base[S]](
                                               id       : S#Id,
                                               inStream : Stream[S, Pat[Event]],
                                               pq       : SkipList.Map[S, TimeRef, Stream[S, Event]],
                                               elem     : S#Var[Event],
                                               _hasNext : S#Var[Boolean],
                                               valid    : S#Var[Boolean]
                                              ) extends Stream[S, Event] {
    protected def typeId: Int = ParImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id       .write(out)
      inStream .write(out)
      pq       .write(out)
      elem     .write(out)
      _hasNext .write(out)
      valid    .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id       .dispose()
      inStream .dispose()
      pq       .dispose()
      elem     .dispose()
      _hasNext .dispose()
      valid    .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      inStream.reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      pq.clear()
      var refCnt = 0
      val _in = inStream
      while (_in.hasNext) {
        val pat = _in.next()
        val it = pat.expand[S]
        if (it.hasNext) {
          pq.put(new TimeRef(refCnt), it)
          refCnt += 1
        }
      }

      advance()
    }

    private def advance()(implicit ctx: Context[S], tx: S#Tx): Unit =
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

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): Event = {
      if (!hasNext) Stream.exhausted()
      val res = elem()
      advance()
      res
    }
  }
}

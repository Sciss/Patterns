/*
 *  Par.scala
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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.impl.TimeRef
import de.sciss.serial.DataOutput

import scala.collection.immutable.{SortedMap => ISortedMap}

/** A simplified version of `Ppar`.*/
final case class Par(in: Pat[Pat[Event]])
  extends Pattern[Event] {

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Event] = new StreamImpl(tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Event] = {
    val inT = t(in)
    if (inT.eq(in)) this else copy(in = inT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, Event] {
    private[this] val id          = tx0.newId()
    private[this] val inStream    = in.expand(ctx, tx0)
    private[this] val pq          = ??? : S#Var[ISortedMap[TimeRef, Stream[S, Event]]] // ctx.newVar[ISortedMap[TimeRef, Stream[S, Event]]](null)(tx0)
    private[this] val _hasNext    = tx0.newBooleanVar(id, false)
    private[this] val elem        = tx0.newVar[Event](id, Event.empty)
    private[this] val _valid      = tx0.newBooleanVar(id, false)

    protected def typeId: Int = ???

    protected def writeData(out: DataOutput): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = ???

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      inStream.reset()
    }

    private def validate()(implicit tx: S#Tx): Unit =
      if (!_valid()) {
        _valid()  = true
        pq()      = ISortedMap.empty

        var refCnt = 0
        inStream.foreach { pat =>
          val it = pat.expand[S]
          if (it.hasNext) {
            pq() = pq() + (new TimeRef(refCnt) -> it)
            refCnt += 1
          }
        }

        advance()
      }

    private def advance()(implicit tx: S#Tx): Unit =
      if (pq().nonEmpty) {
        val (ref, it) = pq().head
        pq()      = pq().tail
        val _elem = it.next()

        elem()    = _elem
        val d     = math.max(0.0, Event.delta(_elem))
        val now   = ref.time
        ref.time += d
        if (it.hasNext) pq() = pq() + (ref -> it)
        if (pq().nonEmpty) {
          val nextTime = pq().firstKey.time
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
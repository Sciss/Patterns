/*
 *  Bind.scala
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

final case class Bind(entries: (String, Pat[_])*) extends Pattern[Event] {
  type EOut[Tx] = Event#Out[Tx]

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, EOut[Tx]] = new StreamImpl(tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, EOut[Tx]] {
    val mapE: Map[String, Stream[Tx, _]] = entries.map {
      case (key, value) => key -> value.expand(ctx, tx0)
    } .toMap  // (breakOut)

    def checkNext()(implicit tx: Tx): Boolean = mapE.forall(_._2.hasNext)

    private[this] val _valid   = ctx.newVar(false)
    private[this] val _hasNext = ctx.newVar(false)

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _hasNext() = checkNext()
        _valid() = true
      }

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

    private def mkState()(implicit tx: Tx): EOut[Tx] = {
      val b = Event.Out.newBuilder
      mapE.foreach {
        case (key, value) =>
          b += key -> value.next()
      }
      b.result()
    }

    def next()(implicit tx: Tx): EOut[Tx] = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = mkState()
      _hasNext() = checkNext()
      res
    }
  }
}

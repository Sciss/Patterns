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
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, Event] = new StreamImpl(tx)

  def transform(t: Transform): Pat[Event] = ???

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, Event] {
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

    private def mkState()(implicit tx: Tx): Event = {
      val m = mapE.map {
        case (key, value) =>
          key -> value.next()
      }
      Event(m)
    }

    def next()(implicit tx: Tx): Event = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = mkState()
      _hasNext() = checkNext()
      res
    }
  }
}

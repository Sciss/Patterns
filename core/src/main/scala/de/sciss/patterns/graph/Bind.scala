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

object Bind {
  type Map = scala.collection.immutable.Map[String, Pat[_]]
  def  Map(elems: (String, Pat[_])*): Map = scala.collection.immutable.Map[String, Pat[_]](elems: _*)

  def apply(map: Map): Bind = Bind(map.toSeq: _*)
}
final case class Bind(entries: (String, Pat[_])*) extends Pattern[Event] {
  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, Event] = new StreamImpl(tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[Event] = {
    val entriesT = entries.map { case (key, value) => key -> t(value) }
    Bind(entriesT: _*)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, Event] {
    private[this] val id        = ctx.newID()(tx0)
    private[this] val _valid    = ctx.newBooleanVar(id, false)(tx0)
    private[this] val _hasNext  = ctx.newBooleanVar(id, false)(tx0)

    val mapE: Map[String, Stream[Tx, _]] = entries.map {
      case (key, value) => key -> value.expand(ctx, tx0)
    } .toMap  // (breakOut)

    def checkNext()(implicit tx: Tx): Boolean = mapE.forall(_._2.hasNext)

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _hasNext() = checkNext()
        _valid() = true
      }

    def reset()(implicit tx: Tx): Unit = if (_valid()) {
      _valid() = false
      mapE.foreach(_._2.reset())
    }

    private def mkState()(implicit tx: Tx): Event = {
      val m = mapE.map {
        case (key, value) =>
          key -> value.next()
      }
      Event(m)
    }

    def next()(implicit tx: Tx): Event = {
      if (!hasNext) Stream.exhausted()
      val res = mkState()
      _hasNext() = checkNext()
      res
    }
  }
}

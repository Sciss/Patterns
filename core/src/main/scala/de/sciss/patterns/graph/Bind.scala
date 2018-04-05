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

import de.sciss.lucre.stm.Base

object Bind {
  type Map = scala.collection.immutable.Map[String, Pat[_]]
  def  Map(elems: (String, Pat[_])*): Map = scala.collection.immutable.Map[String, Pat[_]](elems: _*)

  def apply(map: Map): Bind = Bind(map.toSeq: _*)
}
final case class Bind(entries: (String, Pat[_])*) extends Pattern[Event] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Event] = new StreamImpl(tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Event] = {
    val entriesT = entries.map { case (key, value) => key -> t(value) }
    Bind(entriesT: _*)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, Event] {
    private[this] val id        = tx0.newId()
    private[this] val _valid    = tx0.newBooleanVar(id, false)
    private[this] val _hasNext  = tx0.newBooleanVar(id, false)

    val mapE: Map[String, Stream[S, _]] = entries.map {
      case (key, value) => key -> value.expand(ctx, tx0)
    } .toMap  // (breakOut)

    def checkNext()(implicit tx: S#Tx): Boolean = mapE.forall(_._2.hasNext)

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    private def validate()(implicit tx: S#Tx): Unit =
      if (!_valid()) {
        _hasNext() = checkNext()
        _valid() = true
      }

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      mapE.foreach(_._2.reset())
    }

    private def mkState()(implicit tx: S#Tx): Event = {
      val m = mapE.map {
        case (key, value) =>
          key -> value.next()
      }
      Event(m)
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): Event = {
      if (!hasNext) Stream.exhausted()
      val res = mkState()
      _hasNext() = checkNext()
      res
    }
  }
}

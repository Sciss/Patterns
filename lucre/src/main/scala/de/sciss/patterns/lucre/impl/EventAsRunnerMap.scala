/*
 *  EventAsRunnerMap.scala
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

package de.sciss.patterns.lucre.impl

import de.sciss.lucre.event.Observable
import de.sciss.lucre.expr.IExpr
import de.sciss.lucre.expr.graph.Const
import de.sciss.lucre.stm.{Form, MapLike, Sys}
import de.sciss.patterns.Event
import de.sciss.patterns.graph.AudioCue

final class EventAsRunnerMap[S <: Sys[S]](evt: Event) extends MapLike[S, String, Form] {

  private[this] val map: Map[String, IExpr[S, _]] = evt.map.map {
    case (key, AudioCue(peer))  => (key, new Const.Expanded(peer ): IExpr[S, _])  // XXX TODO ugly!
    case (key, value)           => (key, new Const.Expanded(value): IExpr[S, _])
  }

  def isEmpty(implicit tx: S#Tx): Boolean =
    map.isEmpty

  def nonEmpty(implicit tx: S#Tx): Boolean =
    map.nonEmpty

  def changed: Observable[S#Tx, MapLike.Update[S, String, Form]] = Observable.empty

  def contains(key: String)(implicit tx: S#Tx): Boolean =
    map.contains(key)

  def get(key: String)(implicit tx: S#Tx): Option[Form[S]] =
    map.get(key)

  def dispose()(implicit tx: S#Tx): Unit = ()
}


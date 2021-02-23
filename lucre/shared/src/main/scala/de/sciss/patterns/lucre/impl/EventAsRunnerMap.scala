/*
 *  EventAsRunnerMap.scala
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

package de.sciss.patterns.lucre.impl

import de.sciss.lucre.expr.graph.Const
import de.sciss.lucre.{Form, IExpr, MapObjLike, Observable, Txn}
import de.sciss.patterns.Event
import de.sciss.patterns.graph.AudioCue

final class EventAsRunnerMap[T <: Txn[T]](evt: Event) extends MapObjLike[T, String, Form[T]] {

  private[this] val map: Map[String, IExpr[T, _]] = evt.map.map {
    case (key, AudioCue(peer))  => (key, new Const.Expanded(peer ): IExpr[T, _])  // XXX TODO ugly!
    case (key, value)           => (key, new Const.Expanded(value): IExpr[T, _])
  }

  def isEmpty(implicit tx: T): Boolean =
    map.isEmpty

  def nonEmpty(implicit tx: T): Boolean =
    map.nonEmpty

  def changed: Observable[T, MapObjLike.Update[String, Form[T]]] = Observable.empty

  def contains(key: String)(implicit tx: T): Boolean =
    map.contains(key)

  def get(key: String)(implicit tx: T): Option[Form[T]] =
    map.get(key)

  def dispose()(implicit tx: T): Unit = ()
}


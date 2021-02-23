/*
 *  Bind.scala
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
package graph

import de.sciss.lucre.Exec
import de.sciss.patterns.stream.BindImpl

object Bind {
  type Map = scala.collection.immutable.Map[String, Pat[_]]
  def  Map(elems: (String, Pat[_])*): Map = scala.collection.immutable.Map[String, Pat[_]](elems: _*)

  def apply(map: Map): Bind = Bind(map.toSeq: _*)
}
final case class Bind(entries: (String, Pat[_])*) extends Pattern[Event] {
  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, Event] =
    BindImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[Event] = {
    val entriesT = entries.map { case (key, value) => key -> t(value) }
    Bind(entriesT: _*)
  }
}

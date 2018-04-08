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
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Event] =
    impl.BindImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Event] = {
    val entriesT = entries.map { case (key, value) => key -> t(value) }
    Bind(entriesT: _*)
  }
}

/*
 *  Event.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns

import de.sciss.patterns.Types.Top

object Event {
  private def getOrElseDouble(m: Event#Out, key: String, default: => Double): Double =
    m.get(key) match {
      case Some(d: Double)  => d
      case Some(i: Int)     => i.toDouble
      case _                => default
    }

  def delta(m: Event#Out): Double =
    getOrElseDouble(m, "delta", {
      val stretch = getOrElseDouble(m, "stretch", 1.0)
      val dur     = getOrElseDouble(m, "dur"    , 0.0)
      stretch * dur
    })
}
trait Event extends Top {
  type Out = Map[String, _]
}

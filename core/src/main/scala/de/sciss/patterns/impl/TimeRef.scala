/*
 *  TimeRef.scala
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
package impl

object TimeRef {
  implicit object ord extends Ordering[TimeRef] {
    def compare(x: TimeRef, y: TimeRef): Int =
      if (x.time < y.time) -1 else if (x.time > y.time) 1 else {
        if (x.id < y.id  ) -1 else if (x.id   > y.id  ) 1 else 0
      }
  }
}
final case class TimeRef(id: Int) {
  var time: Double = 0.0
}

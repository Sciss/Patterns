/*
 *  TimeRef.scala
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

package de.sciss.patterns
package impl

import de.sciss.serial.{DataInput, DataOutput, ConstFormat}

object TimeRef {
  implicit object ord extends Ordering[TimeRef] {
    def compare(x: TimeRef, y: TimeRef): Int =
      if (x.time < y.time) -1 else if (x.time > y.time) 1 else {
        if (x.id < y.id  ) -1 else if (x.id   > y.id  ) 1 else 0
      }
  }

  implicit object ser extends ConstFormat[TimeRef] {
    def write(ref: TimeRef, out: DataOutput): Unit = {
      out.writeInt    (ref.id   )
      out.writeDouble (ref.time )
    }

    def read(in: DataInput): TimeRef = {
      val id    = in.readInt()
      val time  = in.readDouble()
      TimeRef(id = id, time = time)
    }
  }
}
final case class TimeRef(id: Int, time: Double = 0.0) {
  def advance(delta: Double): TimeRef = copy(id = id, time = time + delta)
}

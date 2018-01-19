package de.sciss.patterns
package graph
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

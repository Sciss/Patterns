package de.sciss.patterns

import de.sciss.lucre.stm.Plain
import graph._

object PparTest extends App {
  implicit val ctx: Context[Plain] = Context()

  val p1  = Pat( 0 to 10: _*)
  val p2  = Pat(20 to 30: _*)
  val d1  = Pat(1.0 to 2.0 by  0.1: _*)
  val d2  = Pat(2.0 to 1.0 by -0.1: _*)
  val b1  = Bind("pitch" -> p1, "dur" -> d1, "instrument" -> "A")
  val b2  = Bind("pitch" -> p2, "dur" -> d2, "instrument" -> "B")

//  println(s"p1.size = ${p1.expand.size}")
//  println(s"d1.size = ${d1.expand.size}")
  println(s"b1.size = ${b1.expand.toList.size}")

  val par   = Par(Pat(b1, b2))

  val res   = par.expand.toList
  res.foreach(println)
}

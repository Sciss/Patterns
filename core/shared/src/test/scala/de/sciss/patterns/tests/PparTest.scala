package de.sciss.patterns.tests

import de.sciss.lucre.Plain
import de.sciss.patterns.PatImport._
import de.sciss.patterns.graph.{Bind, Par, Pat}
import de.sciss.patterns.Context

object PparTest extends App {
  implicit val ctx: Context[Plain] = Context()

  val p1  = Pat( 0 to 10: _*)
  val p2  = Pat(20 to 30: _*)
//  val d1  = Pat(1.0 to 2.0 by  0.1: _*)
  val d1  = Pat(Vector(1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0): _*)
//  val d2  = Pat(2.0 to 1.0 by -0.1: _*)
  val d2  = Pat(Vector(2.0, 1.9, 1.8, 1.7, 1.6, 1.5, 1.4, 1.3, 1.2, 1.1, 1.0): _*)
  val b1  = Bind("pitch" -> p1, "dur" -> d1, "instrument" -> "A")
  val b2  = Bind("pitch" -> p2, "dur" -> d2, "instrument" -> "B")

//  println(s"p1.size = ${p1.expand.size}")
//  println(s"d1.size = ${d1.expand.size}")
  println(s"b1.size = ${b1.expand.toList.size}")

  val par   = Par(Pat(b1, b2))

  val res   = par.expand.toList
  res.foreach(println)
}

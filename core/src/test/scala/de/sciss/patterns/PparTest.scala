package de.sciss.patterns

import graph._

object PparTest extends App {
  implicit val ctx: Context.Plain = Context()
  import ctx.tx

  val p1  = Pseq( 0 to 10)
  val p2  = Pseq(20 to 30)
  val d1  = Pseq(1.0 to 2.0 by  0.1)
  val d2  = Pseq(2.0 to 1.0 by -0.1)
  val b1  = Bind("pitch" -> p1, "dur" -> d1, "instrument" -> "A")
  val b2  = Bind("pitch" -> p2, "dur" -> d2, "instrument" -> "B")

//  println(s"p1.size = ${p1.expand.size}")
//  println(s"d1.size = ${d1.expand.size}")
  println(s"b1.size = ${b1.expand.toList.size}")

  val par   = Ppar(List(b1, b2))

  val res   = par.expand.toList
  res.foreach(println)
}

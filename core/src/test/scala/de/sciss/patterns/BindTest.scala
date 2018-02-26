package de.sciss.patterns

import graph._

object BindTest extends App {
  implicit val ctx: Context.Plain = Context()
  import ctx.tx

  val b     = Graph {
    val pitch = Brown(30, 140, 4).take(10)
    val dur = Pat.loop(4)(Seq(1.0, 1.2, 1.4))
    Bind("pitch" -> pitch, "dur" -> dur)
  }

  val res   = b.expand.toList
  res.foreach(println)
}

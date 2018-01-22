package de.sciss.patterns

import graph._

object BindTest extends App {
  implicit val ctx: Context.Plain = Context()
  import ctx.tx

  val pitch = Brown(30, 140, 4).take(10)
  val dur   = Pseq(Seq(1.0, 1.2, 1.4), 4)
  val b     = Bind("pitch" -> pitch, "dur" -> dur)

  val res   = b.expand.toList
  res.foreach(println)
}

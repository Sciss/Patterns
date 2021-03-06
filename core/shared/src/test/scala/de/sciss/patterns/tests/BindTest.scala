package de.sciss.patterns.tests

import de.sciss.lucre.Plain
import de.sciss.patterns.PatImport._
import de.sciss.patterns.graph._
import de.sciss.patterns.{Context, Graph}

object BindTest extends App {
  implicit val ctx: Context[Plain] = Context()

  val b     = Graph {
    val pitch = Brown(30, 140, 4).take(10)
    val dur = Pat.loop(4)(Pat(1.0, 1.2, 1.4))
    Bind("pitch" -> pitch, "dur" -> dur)
  }

  val res   = b.expand.toList
  res.foreach(println)
}

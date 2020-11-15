package de.sciss.patterns.tests

import de.sciss.patterns.PatImport._
import de.sciss.patterns.graph

trait InferenceTests {
  import graph._

  def ScalarTest(): Unit = {
    val w               = White(1.0, 2.0)
    val p               = w.toInt
    val q: Pat[Int]     = w.toInt
    val r               = w > 0.5
    val s: Pat[Boolean] = w > 0.5

    Seq(w, p, q, r, s)
  }

  implicit class MyPatOps[A](x: A) {
    def ! : Pat[Seq[A]] = Constant(Seq(x))
  }

  def SeqTest(): Unit = {
    val w                     = White(Constant(Seq(1.0, 1.0)), Constant(Seq(2.0, 2.0)))
    val p                     = w.toInt
    val q: Pat[Seq[Int]]      = w.toInt
    val r                     = w > 0.5.!
    val s: Pat[Seq[Boolean]]  = w > 0.5.!

    Seq(w, p, q, r, s)
  }
}

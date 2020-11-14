package de.sciss.patterns.tests

import de.sciss.lucre.Adjunct.{Num, Widen, Widen2}
import de.sciss.patterns.Pat
import de.sciss.patterns.graph._

trait TypeInferenceTests {
  implicitly[Widen2[Int, Double, Double]]
  implicitly[Widen[Double, Seq[Double]]]
  implicitly[Num[Seq[Double]]]

  val b: Pat[Seq[Double]] = Brown(70.0, 120.0, Constant(Seq(3.0, 4.0)))
  val p: Pat[Seq[Double]] = b.sqrt // midiCps
  val q: Pat[Seq[Double]] = p

  println(q)
}

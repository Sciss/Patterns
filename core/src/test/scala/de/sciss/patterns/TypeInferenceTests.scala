package de.sciss.patterns

import de.sciss.patterns.Types.{Num, Widen, Widen2}
import de.sciss.patterns.graph._

trait TypeInferenceTests {
  implicitly[Widen2[Int, Double, Double]]
  implicitly[Widen[Double, Seq[Double]]]
  implicitly[Num[Seq[Double]]]

  val b: Pat[Seq[Double]] = Brown(70.0, 120.0, Constant(Seq(3.0, 4.0)))
  val p: Pat[Seq[Double]] = b.sqrt // midicps
  val q: Pat[Seq[Double]] = p

  println(q)
}

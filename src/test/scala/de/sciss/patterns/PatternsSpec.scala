package de.sciss.patterns

import de.sciss.patterns.graph._

class PatternsSpec extends PatSpec {
  "A Series" should "produce the expected output for the 'pattern guide' examples" in {
    val pat1 = Series(start = 0, step = 1)
    eval(pat1, 10) shouldBe (0 until 10)
  }

  "White" should "produce the expected output" in {
    val pat1 = White(lo = 2.0, hi = 3.0)
    val values: Seq[Double] = eval(pat1, 30)
    values.foreach { v => assert(v >= 2.0 && v <= 3.0) }
  }

  "Brown" should "produce the expected output" in {
    val pat1 = Brown(lo = 2, hi = 10, step = 4)
    val values: Seq[Int] = eval(pat1, 30)
    val diff = values.sliding(2).map { case Seq(x, y) => y - x } .toList
    values.foreach { v => assert(v >=  2 && v <= 10) }
    diff  .foreach { v => assert(v >= -4 && v <=  4) }
  }
}
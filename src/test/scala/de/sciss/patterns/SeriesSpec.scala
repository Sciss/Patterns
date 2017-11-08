package de.sciss.patterns

import de.sciss.patterns.graph._

class SeriesSpec extends PatSpec {
  "A Series" should "produce the expected output for the 'pattern guide' examples" in {
    val pat1 = Series(start = 0, step = 1)
    eval(pat1, 10) shouldBe (0 until 10)
  }
}

package de.sciss.patterns

import de.sciss.patterns.Types.{IntTop, Top}
import graph._

class FlatMapDebug extends PatSpec {
  "FlatMap" should work in {
    // showStreamLog = true

    val pat = Graph {
      val in = Series(1, 2).take(10).grouped(2)
      in.flatMap { x: Pat.Int => x }
    }

    val plain = (1 to 10).scanLeft(1)((x, _) => x + 2).grouped(2).flatten.toList
    println(plain)

    eval(pat) shouldBe plain
  }
}
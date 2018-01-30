package de.sciss.patterns

import de.sciss.patterns.Types.{IntTop, Top}
import graph._

class FlatMapDebug extends PatSpec {
  "FlatMap" should work in {
    // showStreamLog = true

    val pat1 = Graph {
      val in = Series(1, 2).take(10).grouped(2)
      in.flatMap { x: Pat.Int => x }
    }

    val plain1 = (1 to 9).scanLeft(1)((x, _) => x + 2).grouped(2).flatten.toList
    // println(plain1)

    eval(pat1) shouldBe plain1

    val pat2 = Graph {
      val in = Series(1, 2).take(10).grouped(2)
      in.flatMap { x: Pat.Int => x ++ x }
    }

    val plain2 = (1 to 9).scanLeft(1)((x, _) => x + 2).grouped(2).flatMap(x => x ++ x).toList

    eval(pat2) shouldBe plain2
  }
}
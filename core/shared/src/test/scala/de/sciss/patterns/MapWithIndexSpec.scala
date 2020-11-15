package de.sciss.patterns

import de.sciss.patterns.PatImport._
import de.sciss.patterns.graph.Pat

class MapWithIndexSpec extends PatSpec {
  "MapWithIndex" should work in {
    // it must be possible to ignore the input argument
    val pat1 = Graph {
      val in = Pat(1 to 4: _*).combinations(3)
      in.mapWithIndex { (x, xi) =>
        x ++ xi
      }
    }

    val plain1 = List(1 to 4: _*).combinations(3)
      .zipWithIndex.map { case (x, xi) =>  x :+ xi }
      .toList

    // println(plain1)
    // List(List(1, 2, 3, 0), List(1, 2, 4, 1), List(1, 3, 4, 2), List(2, 3, 4, 3))

    evalH(pat1) shouldBe plain1

    // it must be possible to ignore the input argument
    val pat2 = Graph {
      val in = Pat(1 to 4: _*).combinations(3)
      in.mapWithIndex { (_, xi) =>
        xi ++ xi
      }
    }

    val plain2 = List(1 to 4: _*).combinations(3)
      .zipWithIndex.map { case (_, xi) =>  xi :: xi :: Nil }
      .toList

    evalH(pat2) shouldBe plain2

    // it must be possible to ignore the index argument
    val pat3 = Graph {
      val in = Pat(1 to 4: _*).combinations(3)
      in.mapWithIndex { (x, _) =>
        x.drop(1)
      }
    }

    val plain3 = List(1 to 4: _*).combinations(3)
      .zipWithIndex.map(_._1.drop(1))
      .toList

    evalH(pat3) shouldBe plain3

    // it must be possible to ignore both arguments
    val pat4 = Graph {
      val in = Pat(1 to 4: _*).combinations(3)
      in.mapWithIndex { (_, _) =>
        Pat(6)
      }
    }

    val plain4 = List(1 to 4: _*).combinations(3)
      .zipWithIndex.map(_ => Seq(6))
      .toList

    evalH(pat4) shouldBe plain4
  }
}
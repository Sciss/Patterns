package de.sciss.patterns

class MapSpec extends PatSpec {
  "Map" should work in {
    val pat1 = Graph {
      val in = Pat(1 to 4: _*).combinations(3)
      in.map { x: Pat[Int] =>
        x.drop(1)
      }
    }

    val plain1 = List(1 to 4: _*).combinations(3)
      .map(_.drop(1))
      .toList

    evalH(pat1) shouldBe plain1

    // it must be possible to ignore the function argument
    val pat2 = Graph {
      val in = Pat(1 to 4: _*).combinations(3)
      in.map { _ =>
        Pat(6)
      }
    }

    val plain2 = List(1 to 4: _*).combinations(3)
      .map(_ => Seq(6))
      .toList

    evalH(pat2) shouldBe plain2
  }
}
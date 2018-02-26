package de.sciss.patterns

class ResetSpec extends PatSpec {
  "Map reset" should work in {
    val pat = Graph {
      Pat.loop(2) {
        val in  = Pat(Pat(0, 6, 7))
        val inM = in.map(identity)
        inM
      }
    }

    evalH(pat) shouldBe Seq(Seq(0, 6, 7), Seq(0, 6, 7))
  }

//  "Nested reset" should work in {
//    val pat: Pat[(String, Int)] = Graph {
//      Pat.loop(2) {
//        val x = Pat("foo", "bar", "baz")
//        val i = x.indices.flow()
//        x.bubble.flatMap { s =>
//          s zip i
//        }
//      }
//    }
//
//    eval(pat) shouldBe Seq("foo" -> 0, "bar" -> 1, "baz" -> 2, "foo" -> 0, "bar" -> 1, "baz" -> 2)
//  }

  "Complex nested reset" should work in {
    val plain = Seq(Seq(1), Seq(2, 3), Seq(4, 5)).foldLeft(Seq[Int]()) { (accum, x) =>
      val z   = x.zipWithIndex.map { case (y, xi) =>
        y + xi
      }
      accum ++ z
    }

    val pat: Pat[Int] = Graph {
      Pat(Pat(1), Pat(2, 3), Pat(4, 5)).foldLeft(Pat[Int]()) { (accum, x) =>
        val xi  = x.indices.flow()
        val z   = x.bubble.flatMap { y =>
          y + xi
        }
        accum ++ z
      }
    }

    eval(pat) shouldBe plain
  }
}

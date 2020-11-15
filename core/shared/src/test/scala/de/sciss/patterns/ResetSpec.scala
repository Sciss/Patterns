package de.sciss.patterns

import de.sciss.patterns.PatImport._
import de.sciss.patterns.graph.Pat

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

  "Nested reset" should work in {
    val pat: Pat[(String, Int)] = Graph {
      Pat.loop(2) {
        val x = Pat("foo", "bar", "baz")
        val xz = x zip x.indices
        xz.bubble.flatMap { tup =>
          val (s, i) = tup.unzip
          s zip i
        }
      }
    }

    eval(pat) shouldBe Seq("foo" -> 0, "bar" -> 1, "baz" -> 2, "foo" -> 0, "bar" -> 1, "baz" -> 2)
  }

  "Complex nested reset" should work in {
    val plain = (1 to 2).flatMap { _ =>
      val xs  = Seq(Seq(1), Seq(2, 3))
      val xsi = xs.indices.iterator
      xs.foldLeft(Seq[Int]()) { (accum, x) =>
        val a = xsi.next()
        val z = x.map { y =>
          y + a
        }
        accum ++ z
      }
    }

//    val pat: Pat[Int] = Graph {                 // level 1
//      Pat.loop(2) {                             // level 2
//        val xs  = Pat(Pat(1), Pat(2, 3))
//        val xsi = xs.indices.flow()
//        xs.foldLeft(Pat[Int]()) { (accum, x) => // level 3
//          val xi = xsi.hold()
//          val z  = x.bubble.flatMap { y =>      // level 4
//            y + xi
//          }
//          accum ++ z
//        }
//      }
//    }

    val pat: Pat[Int] = Graph {
      Pat.loop(2) {
        val xs  = Pat(Pat(1), Pat(2, 3))
        val c   = (xs zip xs.indices).bubble
        c.foldLeft(Pat[Int]()) { (accum: Pat[Int], tup: Pat[(Pat[Int], Int)]) =>
          val (xt, xi) = tup.unzip
          val x = xt.flatten
          val z = x.bubble.flatMap { y =>
            y + xi
          }
          accum ++ z
        }
      }
    }

    eval(pat) shouldBe plain
  }
}

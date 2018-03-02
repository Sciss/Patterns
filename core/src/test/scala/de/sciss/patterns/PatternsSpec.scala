package de.sciss.patterns

import de.sciss.patterns.Types.IntTop
import de.sciss.patterns.graph._

class PatternsSpec extends PatSpec {
  "A Series" should "produce the expected output for the 'pattern guide' examples" in {
    val pat1 = ArithmSeq(start = 0, step = 1)
    eval(pat1, 10) shouldBe (0 until 10)
  }

  "White" should work in {
    val pat1 = White(lo = 2.0, hi = 3.0)
    val values1: Seq[Double] = eval(pat1, 30)
    values1.foreach { v => assert(v >= 2.0 && v <= 3.0) }

    val pat2 = Graph {
      Pat.loop(10) {
        val w = White(1, 100).take(4)
        w + w   // should yield even numbers, because the pattern is supposed to expand the same twice
      }
    }

    val values2: Seq[Int] = eval(pat2)
    assert(values2.forall(x => x >= 2 && x <= 200 && (x % 2) == 0))
    // XXX TODO ok, this _could_ fail... We should probably set the RNG seed to ensure it doesn't
    assert(values2.grouped(10).toList.combinations(2).exists {
      case _a :: _b => _a != _b
      case _ => false
    })
  }

  "Brown" should work in {
    val pat1 = Brown(lo = 2, hi = 10, step = 4)
    val values1: Seq[Int] = eval(pat1, 30)
    val diff = values1.sliding(2).map { case Seq(x, y) => y - x } .toList
    values1.foreach { v => assert(v >=  2 && v <= 10) }
    diff   .foreach { v => assert(v >= -4 && v <=  4) }

    // Brown should yield the same sequence from two streams
    val pat2 = Graph {
      val b = Brown(lo = 2, hi = 10, step = 4).take(100)
      b + b // thus all values must thus be even
    }
    val values2: Seq[Int] = eval(pat2)
    assert(values2.forall(x => x >= 4 && x <= 20 && (x % 2 == 0)))
  }

  "Diamond use of a pattern" should "allow multiple stream expansion" in {
    val a = ArithmSeq(9, -1).take(6)
    val b = a.sorted
    val c = a ++ b
    val values: Seq[Int] = eval(c)
    assert(values === Seq(9, 8, 7, 6, 5, 4, 4, 5, 6, 7, 8, 9))
  }

  "Combinations" should work in {
    val p1: Pat[Int] = Pat(1 to 4: _*) // Pseq(1 to 4)
    val p2 = p1.combinations(3)

    val plain = List(1, 2, 3, 4).combinations(3).toList

    evalH(p2) shouldBe plain
  }

  "Differentiate" should work in {
    implicit class SeqOps[A](xs: Seq[A]) {
      // like Kollflitz' `differentiate`
      def differentiate(implicit num: Numeric[A]): List[A] = {
        import num._
        xs.sliding(2).map { case Seq(_a, _b) => _b - _a }.toList
      }
    }

    val s1 = List(5, 6, 2, 10, 8, 0)
    val p1: Pat[Int] = s1
    val p2 = p1.differentiate

    val plain = s1.differentiate

    eval(p2) shouldBe plain
  }

  "Flatten" should work in {
    val p1  = Pat(1 to 4: _*) // Pseq(1 to 4)
    val p2  = p1.combinations(3)
    val values: Seq[Int] = eval(Flatten(p2))
    val plain = List(1, 2, 3, 4).combinations(3).toList.flatten

    assert(values === plain)
  }

  "Copy" should work in {
//    val res1 = Graph {
//      val a = Pat[Int](1, 2, 3).flow()
//      Pat.loop(3) { a }
//    }
//    eval(res1) shouldBe List(1, 2, 3)

    val res1 = Graph {
      val a = Pat[Int](1, 2, 3)
      Pat(a).take(3) flatMap identity
    }
    eval(res1) shouldBe List(1, 2, 3)

    val res2  = Graph {
      val a = Pat[Int](1, 2, 3)
      Pat.loop(3) { a }
    }
    eval(res2) shouldBe List(1, 2, 3, 1, 2, 3, 1, 2, 3)
  }

  "Grouped" should work in {
    val in    = ArithmSeq(1, 3).take(5)
    val res1  = in.bubble
    evalH(res1) shouldBe List(List(1), List(4), List(7), List(10), List(13))

    val res2  = in.grouped(2)
    evalH(res2) shouldBe List(List(1, 4), List(7, 10), List(13))

    val res3  = in.grouped(5)
    evalH(res3) shouldBe List(List(1, 4, 7, 10, 13))

    val res4  = in.grouped(6)
    evalH(res4) shouldBe List(List(1, 4, 7, 10, 13))
  }

  "Sliding" should work in {
    val in    = ArithmSeq(1, 3).take(5)
    val res1  = in.sliding(2)
    evalH(res1) shouldBe List(List(1, 4), List(4, 7), List(7, 10), List(10, 13))

    val res2  = in.sliding(2, 2)
    evalH(res2) shouldBe List(List(1, 4), List(7, 10), List(13))

    val res3  = in.sliding(3, 2)
    evalH(res3) shouldBe List(List(1, 4, 7), List(7, 10, 13))

    val res4  = in.sliding(2, 3)
    evalH(res4) shouldBe List(List(1, 4), List(10, 13))
  }

  "SortWith" should work in {
    val inL     = List(1, 4, 7, 10, 13)
    val in      = ArithmSeq(1, 3).take(5)
    val sorted  = Graph { in.bubble.sortWith((a, b) => a >= b).flatten }
    eval(sorted) shouldBe inL.sorted.reverse
  }

  "FlatTabulate" should work in {
    // must be possible to use iteration variable multiple times
    val pat1 = Graph {
      Pat.loopWithIndex(4) { i =>
        i + i
      }
    }

    val plain1 = (0 until 4).map(i => i + i)

    eval(pat1) shouldBe plain1

    // must be possible to ignore iteration variable
    val pat2 = Graph {
      Pat.loop(4) {
        Pat[Int](6)
      }
    }

    val plain2 = Seq.fill(4)(6)

    eval(pat2) shouldBe plain2
  }
}
package de.sciss.patterns.lucre

import de.sciss.patterns.Pat
import de.sciss.patterns.graph._

class PatternsSpec extends PatSpec {
  "A Series" should "produce the expected output for the 'pattern guide' examples" in {
    val pat1 = Series(start = 0, step = 1)
    eval(pat1, 10) shouldBe (0 until 10)
  }

  "White" should work in {
    val pat1 = White(lo = 2.0, hi = 3.0)
    val values: Seq[Double] = eval(pat1, 30)
    values.foreach { v => assert(v >= 2.0 && v <= 3.0) }
  }

  "Brown" should work in {
    val pat1 = Brown(lo = 2, hi = 10, step = 4)
    val values: Seq[Int] = eval(pat1, 30)
    val diff = values.sliding(2).map { case Seq(x, y) => y - x } .toList
    values.foreach { v => assert(v >=  2 && v <= 10) }
    diff  .foreach { v => assert(v >= -4 && v <=  4) }
  }

  "Diamond use of a pattern" should "allow multiple stream expansion" in {
    val a = Series(9, -1).take(6)
    val b = a.sorted
    val c = a ++ b
    val values: Seq[Int] = eval(c)
    assert(values === Seq(9, 8, 7, 6, 5, 4, 4, 5, 6, 7, 8, 9))
  }

  "Combinations" should work in {
    val p1: Pat.Int = Pseq(1 to 4)
    val p2 = p1.combinations(3)
    val res: List[List[Int]] = ctx.step { implicit tx =>
      p2.expand.map(_.toList).toList
    }

    val plain = List(1, 2, 3, 4).combinations(3).toList

    assert(res === plain)
  }

  "Flatten" should work in {
    val p1  = Pseq(1 to 4)
    val p2  = p1.combinations(3)
    val values: Seq[Int] = eval(Flatten(p2))
    val plain = List(1, 2, 3, 4).combinations(3).toList.flatten

    assert(values === plain)
  }

  "Map" should work in {
    val in = Pseq(1 to 4).combinations(3)
    val pat = in.map { in: Pat.Int =>
      in.drop(1)
    }

    val res = ctx.step { implicit tx =>
      pat.expand.map(_.toList).toList
    }

    val plain = List(1 to 4: _*).combinations(3)
      .map(_.drop(1))
      .toList

    assert(res === plain)
  }
}
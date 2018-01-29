package de.sciss.patterns

import de.sciss.patterns.Types.{IntTop, Top}
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
    import ctx.tx
    val res: List[List[Int]] = p2.expand.map(_.toList).toList

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

    import ctx.tx
    val res = pat.expand.map(_.toList).toList

    val plain = List(1 to 4: _*).combinations(3)
      .map(_.drop(1))
      .toList

    assert(res === plain)
  }

  "Copy" should work in {
    val a     = Pat.Int(1, 2, 3)
    val res1  = Pat.seqFill(3) { _ => a }
    eval(res1) shouldBe List(1, 2, 3)

    val res2  = Pat.seqFill(3) { _ => a.copy() }
    eval(res2) shouldBe List(1, 2, 3, 1, 2, 3, 1, 2, 3)
  }

  "BubbleMap" should work in {
    val resSimple = Pat.Int(1, 2, 3).bubbleMap(x => x ++ Pat.Int(4))
    eval(resSimple) shouldBe List(1, 4, 2, 4, 3, 4)

// XXX TODO
//    val resDup = Pat.Int(1, 2, 3).bubbleMap(x => x ++ x)
//    eval(resDup) shouldBe List(1, 1, 2, 2, 3, 3)

    def directProduct_Seq[A](a: Seq[Seq[A]], b: Seq[A]): Seq[Seq[A]] =
      a.flatMap { v => b.map { w => v :+ w } }

    def directProduct_Pat[A <: Top](a: Pat[Pat[A]], b: Pat[A]): Pat[Pat[A]] =
      a.map { v: Pat[A] =>
        val bc = b.copy()
        bc.bubbleMap { w =>
          v.copy() ++ w
          // bc.take(1) // v.copy() ++ bc.take(1) // w
        }
      }

    val aInSeq  = Seq(Seq(1, 2, 3), Seq(4, 5, 6))
    val bInSeq  = Seq(7, 8)
    val plain   = directProduct_Seq(aInSeq, bInSeq)
    assert(plain === Seq(Seq(1, 2, 3, 7), Seq(1, 2, 3, 8), Seq(4, 5, 6, 7), Seq(4, 5, 6, 8)))

    val aInPat: Pat[Pat.Int]  = aInSeq.map(xs => Pat[IntTop](xs: _*))
    val bInPat: Pat.Int       = Pat[IntTop](bInSeq: _*)
    val outPat = directProduct_Pat(aInPat, bInPat)
    import ctx.tx
    val res = outPat.expand.map { in =>
      in.toList
    }.toList

//    {
//      val v: Pat.Int = Pat[IntTop](aInSeq(0): _*)
//      val res22 = bInPat.bubbleMap { w =>
//        v.copy() ++ w
//        // bc.take(1) // v.copy() ++ bc.take(1) // w
//      }
//      val res23 = res22.expand.toList
//      println(res23)
//    }

    assert(res === plain)  // XXX TODO fails
  }
}
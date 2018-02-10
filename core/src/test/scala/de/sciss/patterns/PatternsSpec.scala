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
    val values: Seq[Double] = eval(pat1, 30)
    values.foreach { v => assert(v >= 2.0 && v <= 3.0) }
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
    val p1: Pat[Int] = Pseq(1 to 4)
    val p2 = p1.combinations(3)

    val plain = List(1, 2, 3, 4).combinations(3).toList

    evalH(p2) shouldBe plain
  }

  "Flatten" should work in {
    val p1  = Pseq(1 to 4)
    val p2  = p1.combinations(3)
    val values: Seq[Int] = eval(Flatten(p2))
    val plain = List(1, 2, 3, 4).combinations(3).toList.flatten

    assert(values === plain)
  }

  "Map" should work in {
    val pat1 = Graph {
      val in = Pseq(1 to 4).combinations(3)
      in.map { x: Pat[Int] =>
        x.drop(1)
      }
    }

    val plain1 = List(1 to 4: _*).combinations(3)
      .map(_.drop(1))
      .toList

    evalH(pat1) shouldBe plain1

    // it must be possible t
    val pat2 = Graph {
      val in = Pseq(1 to 4).combinations(3)
      in.map { _: Pat[Int] =>
        Pat[Int](6)
      }
    }

    val plain2 = List(1 to 4: _*).combinations(3)
      .map(_ => Seq(6))
      .toList

    evalH(pat2) shouldBe plain2
  }

  "Copy" should work in {
    val res1 = Graph {
      val a = Pat[Int](1, 2, 3)
      Pat.seqFill(3) { _ => a }
    }
    eval(res1) shouldBe List(1, 2, 3)

    val res2  = Graph {
      val a = Pat[Int](1, 2, 3)
      Pat.seqFill(3) { _ => a.recur() }
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

  "FoldLeft" should work in {
    val inL   = List(1, 4, 7, 10, 13)
    val in    = ArithmSeq(1, 3).take(5)
    val sum   = Graph { in.bubble.foldLeft(0)(_ + _) }
    eval(sum) shouldBe List(inL.sum)
  }

  "SortWith" should work in {
    val inL     = List(1, 4, 7, 10, 13)
    val in      = ArithmSeq(1, 3).take(5)
    val sorted  = Graph { in.bubble.sortWith((a, b) => a >= b).flatten }
    eval(sorted) shouldBe inL.sorted.reverse
  }

  "SeqFill" should work in {
    // must be possible to use iteration variable multiple times
    val pat1 = Graph {
      Pat.seqFill(4) { i =>
        i + i
      }
    }

    val plain1 = (0 until 4).map(i => i + i)

    eval(pat1) shouldBe plain1

    // must be possible to ignore iteration variable
    val pat2 = Graph {
      Pat.seqFill(4) { _ =>
        Pat[Int](6)
      }
    }

    val plain2 = Seq.fill(4)(6)

    eval(pat2) shouldBe plain2
  }

  // XXX TODO: BubbleMap is broken ATM -- not sure we'll need it much, anyway
//  "BubbleMap" should work in {
//    val resSimple = Graph { Pat[Int](1, 2, 3).bubbleMap(x => x ++ Pat[Int](4)) }
//    eval(resSimple) shouldBe List(1, 4, 2, 4, 3, 4)
//
// XXX TODO
//    val resDup = Pat[Int](1, 2, 3).bubbleMap(x => x ++ x)
//    eval(resDup) shouldBe List(1, 1, 2, 2, 3, 3)
//
//    def directProduct_Seq[A](a: Seq[Seq[A]], b: Seq[A]): Seq[Seq[A]] =
//      a.flatMap { v => b.map { w => v :+ w } }
//
//    def directProduct_Pat[A <: Top](a: Pat[Pat[A]], b: Pat[A]): Pat[Pat[A]] =
//      a.map { v: Pat[A] =>
//        val bc = b.copy()
//        bc.bubbleMap { w =>
//          v.copy() ++ w
//          // bc.take(1) // v.copy() ++ bc.take(1) // w
//        }
//      }
//
//    val aInSeq  = Seq(Seq(1, 2, 3), Seq(4, 5, 6))
//    val bInSeq  = Seq(7, 8)
//    val plain   = directProduct_Seq(aInSeq, bInSeq)
//    assert(plain === Seq(Seq(1, 2, 3, 7), Seq(1, 2, 3, 8), Seq(4, 5, 6, 7), Seq(4, 5, 6, 8)))
//
//    val outPat = Graph {
//      val aInPat: Pat[Pat[Int]]  = aInSeq.map(xs => Pat[IntTop](xs: _*))
//      val bInPat: Pat[Int]       = Pat[IntTop](bInSeq: _*)
//      directProduct_Pat(aInPat, bInPat)
//    }
//    import ctx.tx
//    val res = outPat.expand.map { in =>
//      in.toList
//    }.toList
//
////    {
////      val v: Pat[Int] = Pat[IntTop](aInSeq(0): _*)
////      val res22 = bInPat.bubbleMap { w =>
////        v.copy() ++ w
////        // bc.take(1) // v.copy() ++ bc.take(1) // w
////      }
////      val res23 = res22.expand.toList
////      println(res23)
////    }
//
//    assert(res === plain)  // XXX TODO fails
//  }
}
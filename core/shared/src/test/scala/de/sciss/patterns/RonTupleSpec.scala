package de.sciss.patterns

import de.sciss.lucre.Adjunct.{Num, ScalarEq}
import de.sciss.patterns.PatImport._
import de.sciss.patterns.graph.{Constant, Gate, Hold, Pat}

class RonTupleSpec extends PatSpec {
  // some extra operations
  implicit class SeqOps[A](xs: Seq[A]) {
    // like Kollflitz' `differentiate`
    def differentiate(implicit num: Numeric[A]): Seq[A] = {
      import num._
      xs.sliding(2).map { case Seq(_a, _b) => _b - _a }.toList
    }
  }

  // N.B. SuperCollider `mod` is different from `%` for negative numbers!
  def mod[A](a: A, b: A)(implicit num: Integral[A]): A = {
    import num._
    if (gteq(a, zero)) a % b else {
      val c = -a % b
      if (c == zero) zero else b - c
    }
  }

  def directProduct_Seq[A](a: Seq[Seq[A]], b: Seq[A]): Seq[Seq[A]] =
    a.flatMap { v => b       .map { w => v :+ w }}

  def directProduct_Pat[A](a: Pat[Pat[A]], b: Pat[A]): Pat[Pat[A]] =
    a.flatMap { v => b.bubble.map { w => v ++ w }}

  def extract_Seq[A](s: Seq[A], t: Seq[A]): Seq[Seq[Int]] =
    t.map { tj =>
      s.zipWithIndex.collect {
        case (b, i) if b == tj => i
      }
    }

  def extract_Pat[A: ScalarEq](s: Pat[A], t: Pat[A]): Pat[Pat[Int]] =
    t.bubble.map { tj: Pat[A] =>
      val same      = s sig_== Hold(tj)
      val indices   = s.indices
      Gate(indices, same)
    }

  def allTuples_Seq[A](x: Seq[Seq[A]]): Seq[Seq[A]] = {
    val hd +: tl = x
    val res = tl.foldLeft(hd.map(Seq(_)))((ys, xi) => directProduct_Seq(ys, xi))
    res
  }

  def allTuples_Pat[A](x: Pat[Pat[A]]): Pat[Pat[A]] = {
    val hd = x.head.bubble
    val tl = x.tail
    tl.foldLeft(hd) { (ys: Pat[Pat[A]], xi: Pat[A]) =>
      directProduct_Pat(ys, xi)
    }
  }

  def computeDur_Seq[A](tps: Seq[A], cycle: A)(implicit num: Integral[A]): A = {
    import num._
    val dur0  = tps.differentiate
    val dur1  = dur0.map(mod(_, cycle))
    val dur   = dur1.map { v => if (v == zero) cycle else v }
    val res   = dur.sum
    res
  }

  def computeDur_Pat[A](tps: Pat[A], cycle: Pat[A])(implicit num: Num[A]): Pat[A] = {
    val one  = Constant(num.one)
    val dur0 = tps.differentiate
    val dur  = ((dur0 - one) mod cycle) + one
    dur.sum
  }

  def computeDurs_Seq[A](pattern: Seq[A], cantus: Seq[A], start: Int = 0): Seq[Int] = {
    val positions = extract_Seq(cantus, pattern)
    val tuples0   = allTuples_Seq(positions)
    val tuples    = tuples0.sortWith { (a, b) =>
      val ad = computeDur_Seq(a, 7)
      val bd = computeDur_Seq(b, 7)
      // handle algorithmic ambiguity
      if (ad == bd) {
        (a zip b).collectFirst {
          case (ai, bi) if ai < bi => true
          case (ai, bi) if ai > bi => false
        } .get // OrElse(true)
      } else {
        ad > bd
      }
    }
    val clump     = (Seq(mod(start, cantus.size)) ++ tuples.flatten).sliding(2).toList
    val durs      = clump.map { case Seq(pr0, pr1) =>
      val dur0 = mod(pr1 - pr0, cantus.size)
      if (dur0 == 0) { cantus.size } else dur0
    }
    durs
  }

  def computeDurs_Pat[A: ScalarEq](pattern: Pat[A], cantus: Pat[A], start: Pat[Int] = 0): Pat[Int] = {
    val positions = extract_Pat(cantus, pattern) // <| (_.size.poll("positions.size"))
    val tuples0   = allTuples_Pat(positions)     // <| (_.size.poll("tuple0.size"))
    val tuples    = tuples0.sortWith { (a, b) =>
      val ad = computeDur_Pat(a, 7)
      val bd = computeDur_Pat(b, 7)
      ad > bd
    }
    val cantusSz = cantus.size
    val clump: Pat[Pat[Int]] = ((start % cantusSz) ++ tuples.flatten).sliding(2)
    val durs: Pat[Int] = clump.flatMap { pr =>
      val (pr0, pr1) = pr.splitAt(1)
      val dur0 = ((pr1 - pr0 - 1) mod cantusSz) + 1
      dur0
    }
    durs
  }

  "The directProduct example" should work in {
    //    showStreamLog = true

    val aInSeq  = Seq(Seq(1, 2, 3), Seq(4, 5, 6))
    val bInSeq  = Seq(7, 8)
    val plain   = directProduct_Seq(aInSeq, bInSeq)
    assert(plain === Seq(Seq(1, 2, 3, 7), Seq(1, 2, 3, 8), Seq(4, 5, 6, 7), Seq(4, 5, 6, 8)))

    val outPat = Graph {
      val aInPat: Pat[Pat[Int]]  = Pat(aInSeq.map(xs => Pat[Int](xs: _*)): _*)
      val bInPat: Pat[Int]       = Pat[Int](bInSeq: _*)
      directProduct_Pat(aInPat, bInPat)
    }
    val res = outPat.expand.toIterator.map { in =>
      val i = in.expand.toList
      i
    }.toList

    assert(res === plain)
  }

  "The extract example" should work in {
    val as1     = Seq(1, 5, 2, 3, 4)
    val bs1     = Seq(4, 5, 6)
    val plain1  = extract_Seq(as1, bs1)
    assert(plain1 === List(List(4), List(1), List()))

    val pat1 = Graph {
      extract_Pat(Pat(as1: _*), Pat(bs1: _*))
    }

    evalH(pat1) shouldBe plain1

    val as2     = Seq(-10.4, -3.2, -8.0, -3.2, -0.8, -3.2, -10.4, -10.4)
    val bs2     = Seq(-10.4, -8.0, -3.2)
    val plain2  = extract_Seq(as2, bs2)
    assert(plain2 === Seq(Seq(0, 6, 7), Seq(2), Seq(1, 3, 5)))

    val pat2 = Graph {
      extract_Pat(Pat(as2: _*), Pat(bs2: _*))
    }

    evalH(pat2) shouldBe plain2
  }

  "The allTuples example" should work in {
    def allTuples_Seq1[A](x: Seq[Seq[A]]): Seq[Seq[A]] = {
      val size = x.size
      var res: Seq[Seq[A]] = x.head.map(Seq(_))
      for (i <- 1 until size) {
        res = directProduct_Seq(res, x(i))
      }
      res
    }

    val in  = Seq(Seq(0, 6, 7), Seq(2), Seq(1, 3, 5))
    val out = Seq(Seq(0, 2, 1), Seq(0, 2, 3), Seq(0, 2, 5), Seq(6, 2, 1), Seq(6, 2, 3), Seq(6, 2, 5), Seq(7, 2, 1),
      Seq(7, 2, 3), Seq(7, 2, 5))

    val plainOut1 = allTuples_Seq1(in)
    plainOut1 shouldBe out

    val plainOut2 = allTuples_Seq(in)
    plainOut2 shouldBe out

    val patOut = Graph {
      val inPat0 = in.map(xs => Pat(xs: _*))
      val inPat: Pat[Pat[Int]] = Pat(inPat0: _*)
      allTuples_Pat(inPat)
    }

    evalH(patOut) shouldBe out
  }

  "The computeDur examples" should work in {
    val ex: Seq[((Seq[Int], Int), Int)] = Seq(
      Seq( 5, 4, 3) -> 7 -> 12,
      Seq( 5, 4, 2) -> 7 -> 11,
      Seq( 5, 6, 2) -> 7 ->  4,
      Seq( 5, 6, 3) -> 7 ->  5,
      Seq(10, 8, 0) -> 7 -> 11
    )

    ex.foreach { case ((tps, cycle), res) =>
      val res1 = computeDur_Seq(tps, cycle)
      assert(res1 === res)
    }

    ex.foreach { case ((tps, cycle), res) =>
      val res1 = Graph { computeDur_Pat[Int](Pat(tps: _*), cycle) }
      eval(res1) shouldBe Seq(res)
    }
  }

  "The computeDurs example" should work in {
    val seqIn     = Seq(8.8, 11.2, 16.0)
    val cantus    = Seq(11.2, 11.2, 18.4, 18.4, 16.0, 8.8, 16.0, 16.0)
    val expOut    = Seq(5, 3, 7, 6, 4, 6, 6, 3, 6, 7, 4, 5, 7, 3, 4, 1, 4, 3)

    val plainOut  = computeDurs_Seq(seqIn, cantus)
    assert(plainOut === expOut)

    val patOut = Graph {
//      val patIn     = seqIn .to[Pat]
      val patIn     = Pat(seqIn: _*)
//      val patCantus = cantus.to[Pat]
      val patCantus = Pat(cantus: _*)
      computeDurs_Pat[Double](patIn, patCantus)
    }
    eval(patOut) shouldBe expOut
  }

  "Pat.loop and PatMap.reset" should work in {
    val g = Graph {
      val cantus  = Pat(13.6, 8.8, 6.4, 13.6, 18.4, 16.0, 18.4, 18.4)
      val parts: Pat[Pat[Double]] =
        cantus.distinct.sorted.combinations(3)
      Pat.loop() {
        val bla = parts.map { part =>
          computeDurs_Pat(part, cantus)
        }
        bla(0) // ApplyDebug(bla, 0)
      }
    }

    val gs = eval(g, n = 6)
    gs shouldBe List(2, 7, 7, 2, 7, 2)
//    println(gs.mkString("durs: ", ", ", ""))
  }
}

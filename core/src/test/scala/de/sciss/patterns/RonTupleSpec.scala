package de.sciss.patterns

import de.sciss.patterns.graph.{Gate, Hold}

class RonTupleSpec extends PatSpec {
  def directProduct_Seq[A](a: Seq[Seq[A]], b: Seq[A]): Seq[Seq[A]] =
    a.flatMap { v => b.map { w => v :+ w } }

  def directProduct_Pat[A](a: Pat[Pat[A]], b: Pat[A]): Pat[Pat[A]] =
    a.flatMap { v: Pat[A] =>
      b.bubble.map { w: Pat[A] =>
        v ++ w
      }
    }

  "The directProduct example" should work in {
    //    showStreamLog = true

    val aInSeq  = Seq(Seq(1, 2, 3), Seq(4, 5, 6))
    val bInSeq  = Seq(7, 8)
    val plain   = directProduct_Seq(aInSeq, bInSeq)
    assert(plain === Seq(Seq(1, 2, 3, 7), Seq(1, 2, 3, 8), Seq(4, 5, 6, 7), Seq(4, 5, 6, 8)))

    val outPat = Graph {
      val aInPat: Pat[Pat[Int]]  = aInSeq.map(xs => Pat[Int](xs: _*))
      val bInPat: Pat[Int]       = Pat[Int](bInSeq: _*)
      directProduct_Pat(aInPat, bInPat)
    }
    import ctx.tx
    val res = outPat.expand.map { in =>
      val i = in.expand.toList
      i
    }.toList

    assert(res === plain)  // XXX TODO fails

    /*
      Se we have

        List(List(1, 2, 3, 7), List(8), List(4, 5, 6, 7), List(8))

      instead of

        List(List(1, 2, 3, 7), List(1, 2, 3, 8), List(4, 5, 6, 7), List(4, 5, 6, 8))

      if we create a copy of `v`, the output is

        List(List(1, 2, 3, 7), List(1, 2, 3, 8), List(1, 2, 3, 7), List(1, 2, 3, 8))

     */
  }

  "The extract example" should work in {
    // collects the indices of every occurrence of elements of t in s
    def extract_Seq[A](s: Seq[A], t: Seq[A]): Seq[Seq[Int]] =
      t.map { tj =>
        s.zipWithIndex.collect {
          case (b, i) if b == tj => i
        }
      }

    // collects the indices of every occurrence of elements of t in s
    def extract_Pat[A](s: Pat[A], t: Pat[A]): Pat[Pat[Int]] =
      t.bubble.map { tj: Pat[A] =>
//        val indices   = s.recur().indexOfSlice(tj)
//        val indicesF  = FilterSeq(indices, indices >= 0)
        val same      = s sig_== Hold(tj)
        val indices   = s.indices
        val indicesF  = Gate(indices, same)
        indicesF
      }

    val as1     = Seq(1, 5, 2, 3, 4)
    val bs1     = Seq(4, 5, 6)
    val plain1  = extract_Seq(as1, bs1)
    assert(plain1 === List(List(4), List(1), List()))

    val pat1 = Graph {
      extract_Pat(Pat.Int(as1: _*), Pat.Int(bs1: _*))
    }

    evalH(pat1) shouldBe plain1

    val as2     = Seq(-10.4, -3.2, -8.0, -3.2, -0.8, -3.2, -10.4, -10.4)
    val bs2     = Seq(-10.4, -8.0, -3.2)
    val plain2  = extract_Seq(as2, bs2)
    assert(plain2 === Seq(Seq(0, 6, 7), Seq(2), Seq(1, 3, 5)))

    val pat2 = Graph {
      extract_Pat(Pat.Double(as2: _*), Pat.Double(bs2: _*))
    }

    evalH(pat2) shouldBe plain2
  }

  "The allTuples example" should work in {
    def allTuples_Seq[A](x: Seq[Seq[A]]): Seq[Seq[A]] = {
      val size = x.size
      var res: Seq[Seq[A]] = x.head.map(Seq(_))
      for (i <- 1 until size) {
        res = directProduct_Seq(res, x(i))
      }
      res
    }

    def allTuples_Seq2[A](x: Seq[Seq[A]]): Seq[Seq[A]] = {
      val hd +: tl = x
      val res = tl.foldLeft(hd.map(Seq(_)))((ys, xi) => directProduct_Seq(ys, xi))
      res
    }

    def directProduct_Pat[A](a: Pat[Pat[A]], b: Pat[A]): Pat[Pat[A]] =
      a.flatMap { v: Pat[A] =>
        b.bubble.map { w: Pat[A] =>
          v ++ w
        }
      }

    /*

    List(List(0, 2, 2), List(0, 2, 2), List(0, 2, 2), List(6, 2, 2), List(6, 2, 2), List(6, 2, 2), List(7, 2, 2), List(7, 2, 2), List(7, 2, 2))
    was not equal to
    List(List(0, 2, 1), List(0, 2, 3), List(0, 2, 5), List(6, 2, 1), List(6, 2, 3), List(6, 2, 5), List(7, 2, 1), List(7, 2, 3), List(7, 2, 5))


     */

    // generates all tuplets from within x, an array
    // where each element is an array of occurrences of a value
    def allTuples_Pat[A](x: Pat[Pat[A]]): Pat[Pat[A]] = {
      val hd = x.head.bubble // <| (_.size.poll("hd-sz"))
      val tl = x.tail // .map(_.poll("tl"))// <| (_.size.poll("tl-sz"))
      tl.foldLeft(hd) { (ys: Pat[Pat[A]], xi: Pat[A]) =>
//        ys.map(_.poll("ys")) ++ xi.poll("xi").bubble // directProduct_Pat(ys.map(_.poll("ys")), xi.poll("xi"))
        directProduct_Pat(ys, xi)
      }
    }

    def allTuples_Pat1[A](x: Pat[Pat[A]]): Pat[Pat[A]] = {
      val hd = x.head.bubble
      val tl = x.tail
      val y1 = directProduct_Pat(hd, tl(0))
      val y2 = directProduct_Pat(y1, tl(1))
      y2
    }

    val in  = Seq(Seq(0, 6, 7), Seq(2), Seq(1, 3, 5))
    val out = Seq(Seq(0, 2, 1), Seq(0, 2, 3), Seq(0, 2, 5), Seq(6, 2, 1), Seq(6, 2, 3), Seq(6, 2, 5), Seq(7, 2, 1),
      Seq(7, 2, 3), Seq(7, 2, 5))

    val plainOut1 = allTuples_Seq(in)
    plainOut1 shouldBe out

    val plainOut2 = allTuples_Seq2(in)
    plainOut2 shouldBe out

//    val patOut = Graph {
//      val inPat0 = in.map(xs => Pat.Int(xs: _*))
//      val inPat: Pat[Pat[Int]] = inPat0
//      allTuples_Pat(inPat)
//    }

    val patOut = Graph {
      val inPat0 = in.map(xs => Pat.Int(xs: _*))
      val inPat: Pat[Pat[Int]] = inPat0
      allTuples_Pat(inPat)
    }

    evalH(patOut) shouldBe out
  }
}

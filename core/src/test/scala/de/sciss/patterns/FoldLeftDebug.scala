package de.sciss.patterns

import de.sciss.patterns.graph.{ArithmSeq, PatPat}

class FoldLeftDebug extends PatSpec {
  "FoldLeft" should work in {
    showStreamLog = true

//    Pat(Pat(1), Pat(2), Pat(3)).foldLeft(0)(_ + _)

    val in1   = List(1, 4, 7, 10, 13)
    val pat1  = Graph {
      val inPat = ArithmSeq(1, 3).take(5)
      inPat.bubble.foldLeft(0)(_ + _)
    }
    eval(pat1) shouldBe List(in1.sum)

    val in2 = Seq(Seq(0, 6, 7), Seq(2), Seq(1, 3, 5))
//    val in  = Seq(Seq(1), Seq(2), Seq(3))

    val pat2 = Graph {
      val inPat = in2.map(x => Pat.Int(x: _*)): Pat[Pat[Int]]
      val hd    = inPat.head
      val tl    = inPat.tail
//      tl.foldLeft(hd) { (yi, xi) => yi ++ xi.bubble }
      tl.foldLeft(hd) { (yi, xi) => yi ++ PatPat(xi) }
    }

    evalH(pat2) shouldBe in2
  }
}
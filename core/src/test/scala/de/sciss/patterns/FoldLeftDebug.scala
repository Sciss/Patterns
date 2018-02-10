package de.sciss.patterns

import de.sciss.patterns.graph.PatPat

class FoldLeftDebug extends PatSpec {
  "FoldLeft" should work in {
    showStreamLog = true

    val in  = Seq(Seq(0, 6, 7), Seq(2), Seq(1, 3, 5))
//    val in  = Seq(Seq(1), Seq(2), Seq(3))

    val pat = Graph {
      val inPat = in.map(x => Pat.Int(x: _*)): Pat[Pat[Int]]
      val hd    = inPat.head
      val tl    = inPat.tail
//      tl.foldLeft(hd) { (yi, xi) => yi ++ xi.bubble }
      tl.foldLeft(hd) { (yi, xi) => yi ++ PatPat(xi) }
    }

    evalH(pat) shouldBe in
  }
}
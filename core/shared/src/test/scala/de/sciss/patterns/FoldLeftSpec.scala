package de.sciss.patterns

import de.sciss.patterns.PatImport._
import de.sciss.patterns.graph.Pat

class FoldLeftSpec extends PatSpec {
  "FoldLeft" should work in {
    val in1   = List(1, 4, 7, 10, 13)
    val pat1  = Graph {
      import graph._
      val inPat = ArithmSeq(1, 3).take(5)
      inPat.bubble.foldLeft(0)(_ + _)
    }
    eval(pat1) shouldBe List(in1.sum)

    val in2 = Seq(Seq(0, 6, 7), Seq(2), Seq(1, 3, 5))

    val pat2 = Graph {
      val inPat = Pat(in2.map(x => Pat(x: _*)): _*) // Pat[Pat[Int]]
      val hd    = inPat.take()
      val tl    = inPat.tail
      val foo = tl.foldLeft(hd) { (yi, xi) => yi ++ Pat(xi) }
      foo
    }

    evalH1(pat2) shouldBe in2
  }
}
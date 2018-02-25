package de.sciss.patterns

class FoldLeftSpec extends PatSpec {
  "FoldLeft" should work in {
//    showStreamLog = true

//    Pat(Pat(1), Pat(2), Pat(3)).foldLeft(0)(_ + _)

    val in1   = List(1, 4, 7, 10, 13)
    val pat1  = Graph {
      import graph._
      val inPat = ArithmSeq(1, 3).take(5)
      inPat.bubble.foldLeft(0)(_ + _)
    }
    eval(pat1) shouldBe List(in1.sum)

    val in2 = Seq(Seq(0, 6, 7), Seq(2), Seq(1, 3, 5))
//    val in  = Seq(Seq(1), Seq(2), Seq(3))

    val pat2 = Graph {
      val inPat = in2.map(x => Pat(x: _*)): Pat[Pat[Int]]
      val hd    = inPat.take(1)
      val tl    = inPat.tail
//      val hd    = Pat(Pat(0, 6, 7))
//      val tl    = Pat(Pat(2), Pat(1, 3, 5))
//      tl.foldLeft(hd) { (yi, xi) => yi ++ xi.bubble }
      val foo = tl.foldLeft(hd) { (yi, xi) => yi ++ Pat(xi) }
      foo
    }

    evalH1(pat2) shouldBe in2
//    eval(pat2) shouldBe in2.flatten
  }
}
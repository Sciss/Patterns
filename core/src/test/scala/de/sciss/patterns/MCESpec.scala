package de.sciss.patterns

class MCESpec extends PatSpec {
  import Types._
  import graph._

  "MCE" should work in {
    implicit val ctx: Context.Plain = Context()

    val a = Constant(Seq(1, 2)) + 3
    eval(a, 2) shouldBe List(List(4, 5), List(4, 5))

    val c = Pat(1) + 3
    c.expand: Stream[Tx, Int] // right
    eval(c, 2) shouldBe List(4)

    val d = (1: Pat[Int]) + Constant(Seq(2, 3))
    eval(d, 2) shouldBe List(List(3, 4), List(3, 4))

    val e = Constant(Seq(1, 2)) + Constant(Seq(3, 4))
    eval(e, 2) shouldBe List(List(4, 6), List(4, 6))

    val f = ArithmSeq(Constant(Seq(2, 3)), 4)
    eval(f, 3) shouldBe List(List(2, 3), List(6, 7), List(10, 11))

    val g = (Take("foo", 2) ++ "bar").take(5)
    g.expand: Stream[Tx, String]  // right
    eval(g, 7) shouldBe List("foo", "foo", "bar", "bar", "bar")
  }
}

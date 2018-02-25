package de.sciss.patterns

object Examples {
  import Types._
  import graph._

  def main(args: Array[String]): Unit = run()

  def run(): Unit = {
    implicit val ctx: Context.Plain = Context()
    import ctx.tx

    // ok
    val a = Constant(Seq(1, 2)) + 3
    println(a.expand.take(1).toList.mkString("a: ", ", ", ""))

    // ok
    val b = Constant(Seq(1, 2)) + 3
    println(b.expand.take(1).toList.mkString("b: ", ", ", ""))

    // ok
    val c = Pat.Int(1) + 3
    c.expand: Stream[Unit, Int] // right
    println(c.expand.take(1).toList.mkString("c: ", ", ", ""))

    // ok
    val d = (1: Pat[Int]) + Constant(Seq(2, 3))
    println(d.expand.take(1).toList.mkString("d: ", ", ", ""))

    // ok
    val e = Constant(Seq(1, 2)) + Constant(Seq(3, 4))
    println(e.expand.take(1).toList.mkString("e: ", ", ", ""))

    // ok
    val f = ArithmSeq(Constant(Seq(2, 3)), 4)
    println(f.expand.take(3).toList.mkString("f: ", ", ", ""))

    // ok
    val g = (Take("foo", 2) ++ "bar").take(5)
    g.expand: Stream[Unit, String]  // right
    println(g.expand.take(7).toList.mkString("g: ", ", ", ""))

    //    // ambiguous implicits
    //    Foo(Seq(1, 2), 3.0)

    //    // compiles not
    //    Foo(Seq(1, 2), "foo")
  }
}

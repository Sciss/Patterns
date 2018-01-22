package de.sciss.patterns

object Examples {
  import Types._
  import graph._

  def main(args: Array[String]): Unit = run()

  def run(): Unit = {
    implicit val ctx: Context.Plain = Context()
    import ctx.tx

    // ok
    val a = Add[IntSeqTop, IntTop, IntSeqTop](Seq(1, 2), 3)
    println(a.iterator.take(1).toList.mkString("a: ", ", ", ""))

    // ok
    val b = Add(Seq(1, 2), 3)
    println(b.iterator.take(1).toList.mkString("b: ", ", ", ""))

    // ok
    val c = Add(1, 3)
    c.iterator: Stream[Unit, Int] // right
    println(c.iterator.take(1).toList.mkString("c: ", ", ", ""))

    // ok
    val d = Add(1, Seq(2, 3))
    println(d.iterator.take(1).toList.mkString("d: ", ", ", ""))

    // ok
    val e = Add(Seq(1, 2), Seq(3, 4))
    println(e.iterator.take(1).toList.mkString("e: ", ", ", ""))

    // ok
    val f = Series(Seq(2, 3), 4)
    println(f.iterator.take(3).toList.mkString("f: ", ", ", ""))

    // ok
    val g = (Take("foo", 2) ++ "bar").take(5)
    g.iterator: Stream[Unit, String]  // right
    println(g.iterator.take(7).toList.mkString("g: ", ", ", ""))

    //    // ambiguous implicits
    //    Foo(Seq(1, 2), 3.0)

    //    // compiles not
    //    Foo(Seq(1, 2), "foo")
  }
}

package de.sciss.patterns


object DiamondExample {
  def main(args: Array[String]): Unit = {

    import graph._

    val g = Graph {
      /* def */ val a = Series(9, -1).take(6)
      val b = a.sorted
      a ++ b
    }

    implicit val ctx: Context = Context()

    val it = g.iterator
    val xs = it.toList
    println(s"size = ${xs.size}")
    println(xs.mkString(" "))
  }
}

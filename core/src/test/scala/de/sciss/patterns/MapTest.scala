package de.sciss.patterns

import de.sciss.patterns.graph._

object MapTest extends App {
  implicit val ctx: Context = Context()

  val in = Pseq(1 to 4).combinations(3)
  val pat = in.map { in: Pat.Int =>
    in.drop(1)
  }

  println("-- PAT --")
  val res = pat.expand.map(_.toList).toList
  res.foreach(println)

  val plain = List(1 to 4: _*).combinations(3).map(_.drop(1)).toList
  println("-- LIST --")
  plain.foreach(println)
}
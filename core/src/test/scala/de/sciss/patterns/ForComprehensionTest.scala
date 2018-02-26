package de.sciss.patterns

import de.sciss.patterns.graph._

object ForComprehensionTest extends App {
  val b1 = Graph {
    val lPat = Pat((4 until 8) ++ (8 until 4 by -1): _*).loop()
    lPat.bubble.flatMap { len =>
      val brown = Brown(4, 40, 3)
      brown.grouped(len).flatMap { cantus =>
        cantus :+ -1
      }
    }
  }

  val b2 = Graph {
    val lPat = Pat((4 until 8) ++ (8 until 4 by -1): _*).loop()
    val xs = for {
      len     <- lPat.bubble
      cantus  <- Brown(4, 40, 3).grouped(len)
    } yield {
      cantus :+ -1
    }
    xs.flatten
  }

  println("\n===== b1 =====\n")
  val res1 = {
    implicit val ctx: Context.Plain = Context()
    import ctx.tx
    ctx.setRandomSeed(0L)
    b1.expand.take(30).toList
  }
  res1.foreach(println)

  println("\n===== b2 =====\n")
  val res2 = {
    implicit val ctx: Context.Plain = Context()
    import ctx.tx
    ctx.setRandomSeed(0L)
    b2.expand.take(30).toList
  }
  res2.foreach(println)
}

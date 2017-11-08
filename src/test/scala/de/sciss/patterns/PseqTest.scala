package de.sciss.patterns

import de.sciss.patterns.Types.Top
import de.sciss.patterns.graph._

object PseqTest extends App {
  implicit val ctx: Context = Context()

  def eval[A, T <: Top](p: Pat[T { type Out = A }], expected: Seq[A]): Unit = {
    val gen = p.expand.toSeq
    assert(gen == expected, s"gen = $gen, expected = $expected")
  }

  val p1 = Pseq(Seq(60, 61, 62), 2, 1)
  eval(p1, Seq(61, 62, 60, 61, 62, 60))

  println("Test succeeded.")
}

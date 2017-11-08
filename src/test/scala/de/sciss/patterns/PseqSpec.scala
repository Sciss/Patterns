package de.sciss.patterns

import de.sciss.patterns.Types.Top
import de.sciss.patterns.graph._
import org.scalatest.{FlatSpec, Matchers}

class PseqSpec extends FlatSpec with Matchers {
  implicit val ctx: Context = Context()

  def eval[A, T <: Top](p: Pat[T { type Out = A }]): Seq[A] =
    p.expand.toList

  "A Pseq" should "produce the expected output for a 'flat' input" in {
    val pat = Pseq(Seq(60, 61, 62), 2, 1)
    eval(pat) shouldBe Seq(61, 62, 60, 61, 62, 60)
  }

  it should "produce the expected output for a 'nested' input" in {
    val pat = Pseq(Seq(60, 61, 62), 2, Seq(1, 2))
    eval(pat) shouldBe Seq(Seq(61, 62), Seq(62, 60), Seq(60, 61), Seq(61, 62), Seq(62, 60), Seq(60, 61))
  }
}

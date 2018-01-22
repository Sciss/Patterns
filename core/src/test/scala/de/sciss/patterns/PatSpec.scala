package de.sciss.patterns

import de.sciss.patterns.Types.Top
import org.scalatest.{FlatSpec, Matchers}

trait PatSpec extends FlatSpec with Matchers {
  implicit val ctx: Context.Plain = Context()
  import ctx.tx

  def work: String = "produce the expected output"

  def eval[A, T <: Top](p: Pat.$[T, A], n: Int = Int.MaxValue): Seq[A] = {
    val it0 = p.expand
    val it = if (n == Int.MaxValue) it0 else it0.take(n)
    it.toList
  }
}

package de.sciss.patterns

import org.scalatest.{FlatSpec, Matchers}

trait PatSpec extends FlatSpec with Matchers {
  implicit val ctx: Context.Plain = Context()
  import ctx.tx

  def work: String = "produce the expected output"

  def eval[A](p: Pat[A], n: Int = Int.MaxValue): Seq[A] = {
    val it0: Stream[Unit, A] = p.expand[Unit]
    val it : Stream[Unit, A] = if (n == Int.MaxValue) it0 else it0.take(n)
    it.toList
  }

  def evalH[A](p: Pat[Pat[A]], n: Int = Int.MaxValue): Seq[Seq[A]] = {
    val it0: Stream[Unit, Pat[A]] = p.expand[Unit]
    val it1 = it0.map { in =>
      val inS = in.expand
      inS.toList
    }
    val it : Stream[Unit, Seq[A]] = if (n == Int.MaxValue) it1 else it1.take(n)
    it.toList
  }
}
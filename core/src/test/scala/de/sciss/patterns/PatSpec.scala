package de.sciss.patterns

import de.sciss.patterns.Context.NoTx
import org.scalatest.{FlatSpec, Matchers}

trait PatSpec extends FlatSpec with Matchers {
  type Tx = NoTx

  implicit val ctx: Context.Plain = Context()

  def work: String = "produce the expected output"

  def eval[A](p: Pat[A], n: Int = Int.MaxValue): Seq[A] = {
    val it0: Stream[Tx, A] = p.expand[Tx]
    val it : Stream[Tx, A] = if (n == Int.MaxValue) it0 else it0.take(n)
    it.toList
  }

  def evalH[A](p: Pat[Pat[A]], n: Int = Int.MaxValue): Seq[Seq[A]] = {
    val it0: Stream[Tx, Pat[A]] = p.expand[Tx]
    val it1 = it0.map { in =>
      val inS = in.expand
      inS.toList
    }
    val it : Stream[Tx, Seq[A]] = if (n == Int.MaxValue) it1 else it1.take(n)
    it.toList
  }

  def evalH1[A](p: Pat[Pat[A]], n: Int = Int.MaxValue): Seq[Seq[A]] = {
    val it0: Stream[Tx, Pat[A]] = p.expand[Tx]
    val it1 = it0.toList
    val it  = if (n == Int.MaxValue) it1 else it1.take(n)
    it.map(_.expand.toList)
  }
}
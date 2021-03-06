package de.sciss.patterns

import de.sciss.lucre.Plain
import de.sciss.patterns.graph.Pat
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

trait PatSpec extends AnyFlatSpec with Matchers {
  type S = Plain

  implicit val ctx: Context[Plain] = Context()

  def any2stringadd: Any = ()

  def work: String = "produce the expected output"

  def eval[A](p: Pat[A], n: Int = Int.MaxValue): Seq[A] = {
    val it0 = p.expand[S].toIterator
    val it  = if (n == Int.MaxValue) it0 else it0.take(n)
    it.toList
  }

  def evalH[A](p: Pat[Pat[A]], n: Int = Int.MaxValue): Seq[Seq[A]] = {
    val it0: Stream[S, Pat[A]] = p.expand[S]
    val it1 = it0.toIterator.map { in =>
      val inS = in.expand
      inS.toList
    }
    val it : Iterator[Seq[A]] = if (n == Int.MaxValue) it1 else it1.take(n)
    it.toList
  }

  def evalH1[A](p: Pat[Pat[A]], n: Int = Int.MaxValue): Seq[Seq[A]] = {
    val it0: Stream[S, Pat[A]] = p.expand[S]
    val it1 = it0.toList
    val it  = if (n == Int.MaxValue) it1 else it1.take(n)
    it.map(_.expand.toList)
  }
}
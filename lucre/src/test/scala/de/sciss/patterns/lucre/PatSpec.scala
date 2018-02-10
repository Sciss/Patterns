package de.sciss.patterns.lucre

import de.sciss.patterns.{Pat, Stream}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.stm.InTxn

trait PatSpec extends FlatSpec with Matchers {
  implicit val ctx: Context.InMemory = Context.InMemory()

  type Tx = InTxn

  def work: String = "produce the expected output"

  def eval[A](p: Pat[A], n: Int = Int.MaxValue): Seq[A] =
    ctx.step { implicit tx =>
      val it0: Stream[Tx, A] = p.expand[Tx]
      val it : Stream[Tx, A] = if (n == Int.MaxValue) it0 else it0.take(n)
      it.toList
    }

  def evalH[A](p: Pat[Pat[A]], n: Int = Int.MaxValue): Seq[Seq[A]] = {
    ctx.step { implicit tx =>
      val it0: Stream[Tx, Pat[A]] = p.expand[Tx]
      val it1 = it0.map(_.expand.toList)
      val it: Stream[Tx, Seq[A]] = if (n == Int.MaxValue) it1 else it1.take(n)
      it.toList
    }
  }
}

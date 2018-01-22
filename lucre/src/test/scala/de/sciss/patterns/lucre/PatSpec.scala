package de.sciss.patterns.lucre

import de.sciss.patterns.{Pat, Stream}
import de.sciss.patterns.Types.Top
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.stm.InTxn

trait PatSpec extends FlatSpec with Matchers {
  implicit val ctx: Context.InMemory = Context.InMemory()

  type Tx = InTxn

  def work: String = "produce the expected output"

  def eval[A, T <: Top](p: Pat.$[T, A], n: Int = Int.MaxValue): Seq[A] =
    ctx.step { implicit tx =>
      val it0: Stream[Tx, A] = p.expand[Tx]
      val it : Stream[Tx, A] = if (n == Int.MaxValue) it0 else it0.take(n)
      it.toList
    }
}

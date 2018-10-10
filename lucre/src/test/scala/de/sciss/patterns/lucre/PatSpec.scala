package de.sciss.patterns.lucre

import de.sciss.lucre.stm.InMemory
import de.sciss.patterns
import de.sciss.patterns.{Pat, Stream}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.stm.InTxn

trait PatSpec extends FlatSpec with Matchers {
  type S = InMemory
  implicit val cursor: S = InMemory()
  implicit val ctx: patterns.Context[S] = cursor.step { implicit tx =>
//    val pat = Pattern.empty[S]
    Context[S] // (pat)
  }

  type Tx = InTxn

  def work: String = "produce the expected output"

  def eval[A](p: Pat[A], n: Int = Int.MaxValue): Seq[A] =
    cursor.step { implicit tx =>
      val it0 = p.expand[S].toIterator
      val it  = if (n == Int.MaxValue) it0 else it0.take(n)
      it.toList
    }

  def evalH[A](p: Pat[Pat[A]], n: Int = Int.MaxValue): Seq[Seq[A]] = {
    cursor.step { implicit tx =>
      val it0: Stream[S, Pat[A]] = p.expand[S]
      val it1 = it0.toIterator.map(_.expand.toList)
      val it: Iterator[Seq[A]] = if (n == Int.MaxValue) it1 else it1.take(n)
      it.toList
    }
  }
}

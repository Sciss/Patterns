package de.sciss.patterns.lucre

import de.sciss.lucre.InMemory
import de.sciss.patterns
import de.sciss.patterns.graph.Pat
import de.sciss.patterns.{Stream => PStream}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.stm.InTxn

trait PatSpec extends AnyFlatSpec with Matchers {
  type S = InMemory
  type T = InMemory.Txn
  implicit val cursor: S = InMemory()
  implicit val ctx: patterns.Context[T] = cursor.step { implicit tx =>
    Context[T]()
  }

  type Tx = InTxn

  def work: String = "produce the expected output"

  def eval[A](p: Pat[A], n: Int = Int.MaxValue): Seq[A] =
    cursor.step { implicit tx =>
      val it0 = p.expand[T].toIterator
      val it  = if (n == Int.MaxValue) it0 else it0.take(n)
      it.toList
    }

  def evalH[A](p: Pat[Pat[A]], n: Int = Int.MaxValue): Seq[Seq[A]] = {
    cursor.step { implicit tx =>
      val it0: PStream[T, Pat[A]] = p.expand[T]
      val it1 = it0.toIterator.map(_.expand.toList)
      val it: Iterator[Seq[A]] = if (n == Int.MaxValue) it1 else it1.take(n)
      it.toList
    }
  }
}

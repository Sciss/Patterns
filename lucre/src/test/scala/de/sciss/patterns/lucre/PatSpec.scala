package de.sciss.patterns.lucre

import de.sciss.lucre.stm
import de.sciss.lucre.stm.InMemory
import de.sciss.patterns.{Pat, Stream}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.stm.InTxn

trait PatSpec extends FlatSpec with Matchers {
  type S = InMemory
  implicit val cursor: stm.Cursor[S] = InMemory()
  implicit val ctx: Context[S] = cursor.step { implicit tx => Context[S] }

  type Tx = InTxn

  def work: String = "produce the expected output"

  def eval[A](p: Pat[A], n: Int = Int.MaxValue): Seq[A] =
    cursor.step { implicit tx =>
      val it0: Stream[S, A] = p.expand[S]
      val it : Stream[S, A] = if (n == Int.MaxValue) it0 else it0.take(n)
      it.toList
    }

  def evalH[A](p: Pat[Pat[A]], n: Int = Int.MaxValue): Seq[Seq[A]] = {
    cursor.step { implicit tx =>
      val it0: Stream[S, Pat[A]] = p.expand[S]
      val it1 = it0.map(_.expand.toList)
      val it: Stream[S, Seq[A]] = if (n == Int.MaxValue) it1 else it1.take(n)
      it.toList
    }
  }
}

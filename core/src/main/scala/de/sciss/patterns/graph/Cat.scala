package de.sciss.patterns
package graph

import de.sciss.patterns.Types.{Aux, Widen}

final case class Cat[A1, A2, A](a: Pat[A1], b: Pat[A2])
                               (implicit protected val widen: Widen[A1, A2, A])
  extends Pattern[A] {

  override private[patterns] def aux: List[Aux] = widen :: Nil

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = {
    logStream("Cat.iterator")
    new StreamImpl[Tx](tx)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val ai = a.expand(ctx, tx0) // .map(widen.lift1)
    private[this] val bi = b.expand(ctx, tx0) // .map(widen.lift2)

    def reset()(implicit tx: Tx): Unit = ()

    def hasNext(implicit tx: Tx): Boolean = {
      val res = ai.hasNext || bi.hasNext
//      logStream(s"Cat.iterator.hasNext = $res")
      res
    }

    def next()(implicit tx: Tx): A = {
      val ahn = ai.hasNext
      val res = if (ahn) {
        val aVal = ai.next()
        widen.lift1(aVal)
      } else {
        val bVal = bi.next()
        widen.lift2(bVal)
      }
      logStream(s"Cat.iterator.next(); ai.hasNext = $ahn; res = $res")
      res
    }
  }
}
package de.sciss.patterns
package graph

import de.sciss.lucre.stm.Base
import de.sciss.patterns.Types.{Aux, Widen2}

final case class Cat[A1, A2, A](a: Pat[A1], b: Pat[A2])
                               (implicit w: Widen2[A1, A2, A])
  extends Pattern[A] {

  override private[patterns] def aux: List[Aux] = w :: Nil

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    logStream("Cat.iterator")
    new StreamImpl[S](tx)
  }

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val aT = t(a)
    val bT = t(b)
    if (aT.eq(a) && bT.eq(b)) this else copy(a = aT, b = bT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, A] { stream =>
    private[this] val ai = a.expand(ctx, tx0) // .map(widen.lift1)
    private[this] val bi = b.expand(ctx, tx0) // .map(widen.lift2)

    def reset()(implicit tx: S#Tx): Unit = {
      ai.reset()
      bi.reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      val res = ai.hasNext || bi.hasNext
//      logStream(s"Cat.iterator.hasNext = $res")
      res
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      val ahn = ai.hasNext
      val res = if (ahn) {
        val aVal = ai.next()
        w.widen1(aVal)
      } else {
        val bVal = bi.next()
        w.widen2(bVal)
      }
      logStream(s"Cat.iterator.next(); ai.hasNext = $ahn; res = $res") // ${stream.hashCode().toHexString}
      res
    }
  }
}
package de.sciss.patterns
package graph

import de.sciss.patterns.Types.{Aux, Bridge, Num, Top}

final case class Add[T1 <: Top, T2 <: Top, T <: Top](a: Pat[T1], b: Pat[T2])
                                                    (implicit protected val br: Bridge[T1, T2, T], num: Num[T])
  extends Pattern[T] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = new StreamImpl(tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    private[this] val ai = a.expand(ctx, tx0)
    private[this] val bi = b.expand(ctx, tx0)

    def hasNext(implicit tx: Tx): Boolean = ai.hasNext && bi.hasNext

    def reset()(implicit tx: Tx): Unit = ()

    def next()(implicit tx: Tx): T#Out[Tx] = {
      val an  = br.lift1(ai.next())
      val bn  = br.lift2(bi.next())
      val res = num.plus(an, bn)
      res
    }
  }
}

final case class Cat[T1 <: Top, T2 <: Top, T <: Top](a: Pat[T1], b: Pat[T2])
                                                    (implicit protected val br: Bridge[T1, T2, T])
  extends Pattern[T] {

  override private[patterns] def aux: List[Aux] = br :: Nil

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = {
    logStream("Cat.iterator")
    new StreamImpl[Tx](tx)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    private[this] val ai = a.expand(ctx, tx0).map(br.lift1)
    private[this] val bi = b.expand(ctx, tx0).map(br.lift2)

    def reset()(implicit tx: Tx): Unit = ()

    def hasNext(implicit tx: Tx): Boolean = ai.hasNext || bi.hasNext

    def next()(implicit tx: Tx): T#Out[Tx] = {
      val ahn = ai.hasNext
      val res = if (ahn) ai.next() else bi.next()
      logStream(s"Cat.iterator.next(); ai.hasNext = $ahn; res = $res")
      res
    }
  }
}
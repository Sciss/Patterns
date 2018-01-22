package de.sciss.patterns
package graph

import de.sciss.patterns.Types.{Aux, Bridge, Num, Top}

final case class Add[T1 <: Top, T2 <: Top, T <: Top](a: Pat[T1], b: Pat[T2])
                                                    (implicit protected val br: Bridge[T1, T2, T], num: Num[T])
  extends Pattern[T] {

  def iterator[Tx](implicit ctx: Context[Tx]): Stream[Tx, T#Out[Tx]] = new Stream[Tx, T#Out[Tx]] {
    private[this] val ai = a.expand
    private[this] val bi = b.expand

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

  def iterator[Tx](implicit ctx: Context[Tx]): Stream[Tx, T#Out[Tx]] = {
    val ai = a.expand.map(br.lift1)
    val bi = b.expand.map(br.lift2)
    ai ++ bi
  }
}
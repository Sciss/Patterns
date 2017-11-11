package de.sciss.patterns
package graph

import de.sciss.patterns.Types.{Bridge, Num, Top}

import scala.collection.AbstractIterator

final case class Add[T1 <: Top, T2 <: Top, T <: Top](a: Pat[T1], b: Pat[T2])
                                                    (implicit protected val br: Bridge[T1, T2, T], num: Num[T#Out])
  extends Pattern[T] {

  def iterator(implicit ctx: Context): Iterator[T#Out] = {
    val ai = a.expand
    val bi = b.expand

    new AbstractIterator[T#Out] {
      def hasNext: Boolean = ai.hasNext && bi.hasNext

      def next(): T#Out = {
        val an  = br.lift1(ai.next())
        val bn  = br.lift2(bi.next())
        val res = num.plus(an, bn)
        res
      }
    }
  }
}

final case class Cat[T1 <: Top, T2 <: Top, T <: Top](a: Pat[T1], b: Pat[T2])
                                                    (implicit protected val br: Bridge[T1, T2, T])
  extends Pattern[T] {

  def iterator(implicit ctx: Context): Iterator[T#Out] = {
    val ai = a.expand.map(br.lift1)
    val bi = b.expand.map(br.lift2)
    ai ++ bi
  }
}
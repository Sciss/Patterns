/*
 *  Patterns.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package graph

import de.sciss.patterns.Types.{Bridge, Num, Top}
import de.sciss.patterns.graph.impl.SeriesLike

import scala.collection.AbstractIterator
import scala.util.Random

/** A pattern that generates an arithmetic series. Corresponds to `Pseries` in SuperCollider,
  * but does not have a `length` arguments. Use `.take(N)` instead.
  */
final case class Series[T1 <: Top, T2 <: Top, T <: Top](start: Pat[T1], step: Pat[T2])
                                                       (implicit protected val br: Bridge[T1, T2, T], num: Num[T#Out])
  extends SeriesLike[T1, T2, T] {

  protected def op(a: T#Out, b: T#Out): T#Out = num.plus(a, b)
}

final case class Geom[T1 <: Top, T2 <: Top, T <: Top](start: Pat[T1], step: Pat[T2])
                                                     (implicit protected val br: Bridge[T1, T2, T], num: Num[T#Out])
  extends SeriesLike[T1, T2, T] {

  protected def op(a: T#Out, b: T#Out): T#Out = num.times(a, b)
}

final case class Brown[T1 <: Top, T2 <: Top, T <: Top](lo: Pat[T1], hi: Pat[T1], step: Pat[T2])
                                                      (implicit protected val br: Bridge[T1, T2, T], num: Num[T#Out])
  extends Pattern[T] {

  protected def calcNext(cur: T#Out, step: T#Out)(implicit r: Random): T#Out = {
    num.plus(cur, num.rand2(step))
  }

  def iterator(implicit ctx: Context): Iterator[T#Out] = {
    val loIt = lo.expand.map(br.lift1)
    val hiIt = hi.expand.map(br.lift1)

    if (loIt.isEmpty || hiIt.isEmpty) Iterator.empty
    else new AbstractIterator[T#Out] {
      private[this] implicit val r: Random  = ctx.mkRandom()
      private[this] var state: T#Out        = num.rrand(loIt.next(), hiIt.next())
      private[this] val stepIt              = step.expand
      private[this] val loVal               = loIt.next()
      private[this] val hiVal               = hiIt.next()

      var hasNext = true

      def next(): T#Out = {
        val res = state
        hasNext = loIt.hasNext && hiIt.hasNext && stepIt.hasNext
        if (hasNext) {
          val x = calcNext(state, br.lift2(stepIt.next()))
          state = num.fold(x, loVal, hiVal)
        }
        res
      }
    }
  }
}
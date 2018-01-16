/*
 *  Patterns.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
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

import scala.util.Random

/** A pattern that generates an arithmetic series. Corresponds to `Pseries` in SuperCollider,
  * but does not have a `length` arguments. Use `.take(N)` instead.
  */
final case class Series[T1 <: Top, T2 <: Top, T <: Top](start: Pat[T1], step: Pat[T2])
                                                       (implicit protected val br: Bridge[T1, T2, T], num: Num[T])
  extends SeriesLike[T1, T2, T] {

  protected def op(a: T#Out, b: T#Out): T#Out = num.plus(a, b)
}

final case class Geom[T1 <: Top, T2 <: Top, T <: Top](start: Pat[T1], step: Pat[T2])
                                                     (implicit protected val br: Bridge[T1, T2, T], num: Num[T])
  extends SeriesLike[T1, T2, T] {

  protected def op(a: T#Out, b: T#Out): T#Out = num.times(a, b)
}

final case class Brown[T1 <: Top, T2 <: Top, T <: Top](lo: Pat[T1], hi: Pat[T1], step: Pat[T2])
                                                      (implicit protected val br: Bridge[T1, T2, T], num: Num[T])
  extends Pattern[T] {

  protected def calcNext(cur: T#Out, step: T#Out)(implicit r: Random): T#Out = {
    num.plus(cur, num.rand2(step))
  }

  def iterator(implicit ctx: Context): Stream[T#Out] = new Stream[T#Out] {
    private[this] val loStream    = lo.expand.map(br.lift1)
    private[this] val hiStream    = hi.expand.map(br.lift1)
    private[this] val stepStream  = step.expand

    private[this] implicit val r: Random  = ctx.mkRandom()

    private[this] var state   : T#Out   = _
    private[this] var _hasNext: Boolean = _

    def hasNext: Boolean = _hasNext

    def reset(): Unit = {
      _hasNext = loStream.hasNext && hiStream.hasNext
      if (_hasNext) {
        state = num.rrand(loStream.next(), hiStream.next())
      }
    }

    reset()

    def next(): T#Out = {
      if (!_hasNext) Stream.exhausted()
      val res = state
      _hasNext = loStream.hasNext && hiStream.hasNext && stepStream.hasNext
      if (_hasNext) {
        val loVal   = loStream.next()
        val hiVal   = hiStream.next()
        val stepVal = stepStream.next()
        val x       = calcNext(state, br.lift2(stepVal))
        state       = num.fold(x, loVal, hiVal)
      }
      res
    }
  }
}

final case class White[T <: Top](lo: Pat[T], hi: Pat[T])(implicit num: Num[T])
  extends Pattern[T] {

  def iterator(implicit ctx: Context): Stream[T#Out] = {
    val loIt = lo.expand
    val hiIt = hi.expand

    if (loIt.isEmpty || hiIt.isEmpty) Stream.empty
    else new Stream[T#Out] {
      private[this] implicit val r: Random = ctx.mkRandom()

      private def mkState(): T#Out = num.rrand(loIt.next(), hiIt.next())

      private[this] var state: T#Out = mkState()

      var hasNext = true

      def reset(): Unit = ???

      def next(): T#Out = {
        val res = state
        hasNext = loIt.hasNext && hiIt.hasNext
        if (hasNext) {
          state = mkState()
        }
        res
      }
    }
  }
}
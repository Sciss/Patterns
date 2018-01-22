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

import de.sciss.patterns.Types.{Aux, Bridge, Num, Top}
import de.sciss.patterns.graph.impl.SeriesLike

/** A pattern that generates an arithmetic series. Corresponds to `Pseries` in SuperCollider,
  * but does not have a `length` arguments. Use `.take(N)` instead.
  */
final case class Series[T1 <: Top, T2 <: Top, T <: Top](start: Pat[T1], step: Pat[T2])
                                                       (implicit protected val br: Bridge[T1, T2, T], num: Num[T])
  extends SeriesLike[T1, T2, T] {

  override private[patterns] def aux: List[Aux] = br :: num :: Nil

  protected def op[Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx] = num.plus(a, b)
}

final case class Geom[T1 <: Top, T2 <: Top, T <: Top](start: Pat[T1], step: Pat[T2])
                                                     (implicit protected val br: Bridge[T1, T2, T], num: Num[T])
  extends SeriesLike[T1, T2, T] {

  override private[patterns] def aux: List[Aux] = br :: num :: Nil

  protected def op[Tx](a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx] = num.times(a, b)
}

final case class Brown[T1 <: Top, T2 <: Top, T <: Top](lo: Pat[T1], hi: Pat[T1], step: Pat[T2])
                                                      (implicit protected val br: Bridge[T1, T2, T], num: Num[T])
  extends Pattern[T] {

  override private[patterns] def aux: List[Aux] = br :: num :: Nil

  protected def calcNext[Tx](cur: T#Out[Tx], step: T#Out[Tx])(implicit r: Random[Tx], tx: Tx): T#Out[Tx] = {
    num.plus(cur, num.rand2(step))
  }

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = new StreamImpl(tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends Stream[Tx, T#Out[Tx]] {

    // println("Brown.iterator")
    // (new Exception).fillInStackTrace().printStackTrace()

    private[this] val loStream    = lo  .expand(ctx, tx0).map(br.lift1)
    private[this] val hiStream    = hi  .expand(ctx, tx0).map(br.lift1)
    private[this] val stepStream  = step.expand(ctx, tx0)

    private[this] implicit val r: Random[Tx] = ctx.mkRandom()(tx0)

    private[this] val state     = ctx.newVar[T#Out[Tx]](null.asInstanceOf[T#Out[Tx]])
    private[this] val _hasNext  = ctx.newVar(false)
    private[this] val _valid    = ctx.newVar(false)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid() = true
        _hasNext() = loStream.hasNext && hiStream.hasNext
        if (_hasNext()) {
          state() = num.rrand(loStream.next(), hiStream.next())
        }
      }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

    def next()(implicit tx: Tx): T#Out[Tx] = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = state()
      _hasNext() = loStream.hasNext && hiStream.hasNext && stepStream.hasNext
      if (_hasNext()) {
        val loVal   = loStream.next()
        val hiVal   = hiStream.next()
        val stepVal = stepStream.next()
        val x       = calcNext(res, br.lift2(stepVal))
        state()     = num.fold(x, loVal, hiVal)
      }
      res
    }
  }
}

final case class White[T <: Top](lo: Pat[T], hi: Pat[T])(implicit num: Num[T])
  extends Pattern[T] {

  override private[patterns] def aux: List[Aux] = num :: Nil

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    private[this] val loStream  = lo.expand(ctx, tx0)
    private[this] val hiStream  = hi.expand(ctx, tx0)

    private[this] implicit val r: Random[Tx] = ctx.mkRandom()(tx0)

    private def mkState()(implicit tx: Tx): T#Out[Tx] = num.rrand(loStream.next(), hiStream.next())

    private[this] val state     = ctx.newVar[T#Out[Tx]](null.asInstanceOf[T#Out[Tx]])
    private[this] val _hasNext  = ctx.newVar(false)
    private[this] val _valid    = ctx.newVar(false)

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid() = true
        _hasNext() = loStream.hasNext && hiStream.hasNext
        if (_hasNext()) {
          state() = num.rrand(loStream.next(), hiStream.next())
        }
      }

    def next()(implicit tx: Tx): T#Out[Tx] = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = state()
      _hasNext() = loStream.hasNext && hiStream.hasNext
      if (_hasNext()) {
        state() = mkState()
      }
      res
    }
  }
}
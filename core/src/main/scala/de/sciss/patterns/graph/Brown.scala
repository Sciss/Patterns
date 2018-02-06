/*
 *  Brown.scala
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

import de.sciss.patterns.Types.{Aux, Num, Top, Widen}

final case class Brown[T1 <: Top, T2 <: Top, T <: Top](lo: Pat[T1], hi: Pat[T1], step: Pat[T2])
                                                      (implicit protected val widen: Widen[T1, T2, T], num: Num[T])
  extends Pattern[T] { pat =>

  override private[patterns] def aux: List[Aux] = widen :: num :: Nil

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = new StreamImpl(tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends Stream[Tx, T#Out[Tx]] {

    // println("Brown.iterator")
    // (new Exception).fillInStackTrace().printStackTrace()

    private[this] val loStream    = lo  .expand(ctx, tx0).map(widen.lift1)
    private[this] val hiStream    = hi  .expand(ctx, tx0).map(widen.lift1)
    private[this] val stepStream  = step.expand(ctx, tx0)

    private[this] implicit val r: Random[Tx] = ctx.mkRandom(pat.ref)(tx0)

    private[this] val state     = ctx.newVar[T#Out[Tx]](null.asInstanceOf[T#Out[Tx]])
    private[this] val _hasNext  = ctx.newVar(false)
    private[this] val _valid    = ctx.newVar(false)

    @inline
    private def calcNext(cur: T#Out[Tx], step: T#Out[Tx])(implicit r: Random[Tx], tx: Tx): T#Out[Tx] =
      num.plus(cur, num.rand2(step))

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
        val x       = calcNext(res, widen.lift2(stepVal))
        state()     = num.fold(x, loVal, hiVal)
      }
      res
    }
  }
}

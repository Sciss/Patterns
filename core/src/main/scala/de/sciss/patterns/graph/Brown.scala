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

import de.sciss.patterns.Types.{Aux, Num, Widen}

final case class Brown[A1, A2, A](lo: Pat[A1], hi: Pat[A1], step: Pat[A2])
                                 (implicit protected val widen: Widen[A1, A2, A], num: Num[A])
  extends Pattern[A] { pat =>

  override private[patterns] def aux: List[Aux] = widen :: num :: Nil

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl(tx)

  def transform(t: Transform): Pat[A] = {
    val loT   = t(lo)
    val hiT   = t(hi)
    val stepT = t(step)
    if (loT.eq(lo) && hiT.eq(hi) && stepT.eq(step)) this else copy(lo = loT, hi = hiT, step = stepT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends Stream[Tx, A] {

    // println("Brown.iterator")
    // (new Exception).fillInStackTrace().printStackTrace()

    private[this] val loStream    = lo  .expand(ctx, tx0).map(widen.lift1)
    private[this] val hiStream    = hi  .expand(ctx, tx0).map(widen.lift1)
    private[this] val stepStream  = step.expand(ctx, tx0)

    private[this] implicit val r: Random[Tx] = ctx.mkRandom(pat.ref)(tx0)

    private[this] val state     = ctx.newVar[A](null.asInstanceOf[A])
    private[this] val _hasNext  = ctx.newVar(false)
    private[this] val _valid    = ctx.newVar(false)

    @inline
    private def calcNext(cur: A, step: A)(implicit r: Random[Tx], tx: Tx): A =
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

    def next()(implicit tx: Tx): A = {
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

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

import de.sciss.patterns.Context.Var
import de.sciss.patterns.Types.{Aux, Num, Widen2}

final case class Brown[A1, A2, A](lo: Pat[A1], hi: Pat[A1], step: Pat[A2])
                                 (implicit w: Widen2[A1, A2, A], num: Num[A])
  extends Pattern[A] { pat =>

  override private[patterns] def aux: List[Aux] = w :: num :: Nil

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl(tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val loT   = t(lo)
    val hiT   = t(hi)
    val stepT = t(step)
    if (loT.eq(lo) && hiT.eq(hi) && stepT.eq(step)) this else copy(lo = loT, hi = hiT, step = stepT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends Stream[Tx, A] {

    private[this] val id          = ctx.newID()(tx0)

    private[this] val loStream    = lo  .expand(ctx, tx0).map(w.widen1)(ctx, tx0)
    private[this] val hiStream    = hi  .expand(ctx, tx0).map(w.widen1)(ctx, tx0)
    private[this] val stepStream  = step.expand(ctx, tx0)

    private[this] implicit val r: Random[Tx] = ctx.mkRandom(pat.ref)(tx0)

    private[this] val state       = ??? : Var[Tx, A] // ctx.newVar[A](null.asInstanceOf[A])(tx0)
    private[this] val _hasNext    = ctx.newBooleanVar(id, false)(tx0)
    private[this] val _valid      = ctx.newBooleanVar(id, false)(tx0)

    @inline
    private def calcNext(cur: A, step: A)(implicit r: Random[Tx], tx: Tx): A =
      num.+(cur, num.rand2(step))

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

    def reset()(implicit tx: Tx): Unit = if (_valid()) {
      _valid() = false
      loStream  .reset()
      hiStream  .reset()
      stepStream.reset()
      // XXX TODO: r.reset()
    }

    def next()(implicit tx: Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = state()
      _hasNext() = loStream.hasNext && hiStream.hasNext && stepStream.hasNext
      if (_hasNext()) {
        val loVal   = loStream.next()
        val hiVal   = hiStream.next()
        val stepVal = stepStream.next()
        val x       = calcNext(res, w.widen2(stepVal))
        state()     = num.fold(x, loVal, hiVal)
      }
      res
    }
  }
}

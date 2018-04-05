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

import de.sciss.lucre.stm.{Base, Random}
import de.sciss.patterns.Types.{Aux, Num, Widen2}
import de.sciss.patterns.impl.PatElem

final case class Brown[A1, A2, A](lo: Pat[A1], hi: Pat[A1], step: Pat[A2])
                                 (implicit val widen: Widen2[A1, A2, A], val num: Num[A])
  extends Pattern[A] { pat =>

  override private[patterns] def aux: List[Aux] = widen :: num :: Nil

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new StreamImpl(tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val loT   = t(lo)
    val hiT   = t(hi)
    val stepT = t(step)
    if (loT.eq(lo) && hiT.eq(hi) && stepT.eq(step)) this else copy(lo = loT, hi = hiT, step = stepT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S])
    extends Stream[S, A] {

    private[this] val id          = tx0.newId()

    private[this] val loStream    = lo  .expand(ctx, tx0).map(widen.widen1)(ctx, tx0)
    private[this] val hiStream    = hi  .expand(ctx, tx0).map(widen.widen1)(ctx, tx0)
    private[this] val stepStream  = step.expand(ctx, tx0)

    private[this] implicit val r: Random[S#Tx] = ctx.mkRandom(pat.ref)(tx0)

    private[this] val state       = PatElem.makeVar[S, A](id)(tx0)
    private[this] val _hasNext    = tx0.newBooleanVar(id, false)
    private[this] val _valid      = tx0.newBooleanVar(id, false)

    @inline
    private def calcNext(cur: A, step: A)(implicit r: Random[S#Tx], tx: S#Tx): A =
      num.+(cur, num.rand2(step))

    private def validate()(implicit tx: S#Tx): Unit =
      if (!_valid()) {
        _valid() = true
        _hasNext() = loStream.hasNext && hiStream.hasNext
        if (_hasNext()) {
          state() = num.rrand(loStream.next(), hiStream.next())
        }
      }

    def hasNext(implicit tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      loStream  .reset()
      hiStream  .reset()
      stepStream.reset()
      // XXX TODO: r.reset()
    }

    def next()(implicit tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = state()
      _hasNext() = loStream.hasNext && hiStream.hasNext && stepStream.hasNext
      if (_hasNext()) {
        val loVal   = loStream.next()
        val hiVal   = hiStream.next()
        val stepVal = stepStream.next()
        val x       = calcNext(res, widen.widen2(stepVal))
        state()     = num.fold(x, loVal, hiVal)
      }
      res
    }
  }
}

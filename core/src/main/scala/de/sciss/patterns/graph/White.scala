/*
 *  White.scala
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
import de.sciss.patterns.Types.{Aux, Num}

final case class White[A](lo: Pat[A], hi: Pat[A])(implicit num: Num[A])
  extends Pattern[A] { pat =>

  override private[patterns] def aux: List[Aux] = num :: Nil

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl[Tx](tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val loT = t(lo)
    val hiT = t(hi)
    if (loT.eq(lo) && hiT.eq(hi)) this else copy(lo = loT, hi = hiT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val id        = ctx.newID()(tx0)
    private[this] val loStream  = lo.expand(ctx, tx0)
    private[this] val hiStream  = hi.expand(ctx, tx0)
    private[this] val state     = ??? : Var[Tx, A] // ctx.newVar[A](null.asInstanceOf[A])(tx0)
    private[this] val _hasNext  = ctx.newBooleanVar(id, false)(tx0)
    private[this] val _valid    = ctx.newBooleanVar(id, false)(tx0)

    private[this] implicit val r: Random[Tx] = ctx.mkRandom(pat.ref)(tx0)

    private def mkState()(implicit tx: Tx): A = num.rrand(loStream.next(), hiStream.next())

    def reset()(implicit tx: Tx): Unit = if (_valid()) {
      _valid() = false
      loStream.reset()
      hiStream.reset()
      // XXX TODO: r.reset()
    }

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

    def next()(implicit tx: Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = state()
      _hasNext() = loStream.hasNext && hiStream.hasNext
      if (_hasNext()) {
        state() = mkState()
      }
      res
    }
  }
}
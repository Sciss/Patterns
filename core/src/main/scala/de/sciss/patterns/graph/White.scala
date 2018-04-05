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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.Types.{Aux, Num}
import de.sciss.patterns.impl.PatElem

final case class White[A](lo: Pat[A], hi: Pat[A])(implicit num: Num[A])
  extends Pattern[A] { pat =>

  override private[patterns] def aux: List[Aux] = num :: Nil

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val loT = t(lo)
    val hiT = t(hi)
    if (loT.eq(lo) && hiT.eq(hi)) this else copy(lo = loT, hi = hiT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, A] {
    private[this] val id        = tx0.newId()
    private[this] val loStream  = lo.expand(ctx, tx0)
    private[this] val hiStream  = hi.expand(ctx, tx0)
    private[this] val state     = PatElem.makeVar[S, A](id)(tx0)
    private[this] val _hasNext  = tx0.newBooleanVar(id, false)
    private[this] val _valid    = tx0.newBooleanVar(id, false)

    private[this] implicit val r: Random[S#Tx] = ctx.mkRandom(pat.ref)(tx0)

    private def mkState()(implicit tx: S#Tx): A = num.rrand(loStream.next(), hiStream.next())

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      loStream.reset()
      hiStream.reset()
      // XXX TODO: r.reset()
    }

    def hasNext(implicit tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    private def validate()(implicit tx: S#Tx): Unit =
      if (!_valid()) {
        _valid() = true
        _hasNext() = loStream.hasNext && hiStream.hasNext
        if (_hasNext()) {
          state() = num.rrand(loStream.next(), hiStream.next())
        }
      }

    def next()(implicit tx: S#Tx): A = {
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
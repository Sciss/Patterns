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

import de.sciss.patterns.Types.{Aux, Num, Top}

final case class White[T <: Top](lo: Pat[T], hi: Pat[T])(implicit num: Num[T])
  extends Pattern[T] { pat =>

  override private[patterns] def aux: List[Aux] = num :: Nil

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    private[this] val loStream  = lo.expand(ctx, tx0)
    private[this] val hiStream  = hi.expand(ctx, tx0)

    private[this] implicit val r: Random[Tx] = ctx.mkRandom(pat.ref)(tx0)

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
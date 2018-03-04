/*
 *  Sum.scala
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

import de.sciss.patterns.Types.{Aux, Num}

final case class Sum[A](in: Pat[A])(implicit num: Num[A]) extends Pattern[A] {
  override private[patterns] def aux: List[Aux] = num :: Nil

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl[Tx](tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val inT = t(in)
    if (inT.eq(in)) this else copy(in = inT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val inStream  = in.expand(ctx, tx0)
    private[this] val _valid    = ctx.newVar(false)
    private[this] val _hasNext  = ctx.newVar(false)
    private[this] val state     = ctx.newVar[A](null.asInstanceOf[A])

    def reset()(implicit tx: Tx): Unit = if (_valid()) {
      _valid() = false
      inStream.reset()
    }

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid() = true
        val ihn = inStream.hasNext
        _hasNext() = ihn
        if (ihn) {
          var acc = inStream.next()
          while (inStream.hasNext) {
            acc = num.+(acc, inStream.next())
          }
          state() = acc
        }
      }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = state()
      _hasNext() = false
      res
    }
  }
}
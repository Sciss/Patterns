/*
 *  Sorted.scala
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
import de.sciss.patterns.Types.{Aux, ScalarOrd}

final case class Sorted[A](in: Pat[A])(implicit ord: ScalarOrd[A]) extends Pattern[A] {
  override private[patterns] def aux: List[Aux] = ord :: Nil

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl(tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val inT = t(in)
    if (inT eq in) this else copy(in = inT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val inStream  = in.expand(ctx, tx0)
    private[this] val _valid    = ctx.newBooleanVar(false)(tx0)
    private[this] val sortedIt  = ??? : Var[Tx, Stream[Tx, A]] // ctx.newVar[Stream[Tx, A]](null)(tx0)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()    = true
        val xs      = inStream.toList
        sortedIt()  = Stream(xs.sortWith(ord.lt): _*)
      }

    def reset()(implicit tx: Tx): Unit = if (_valid()) {
      _valid() = false
      inStream.reset()
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      sortedIt().hasNext
    }

    def next()(implicit tx: Tx): A = {
      validate()
      sortedIt().next()
    }
  }
}

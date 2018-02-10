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

import de.sciss.patterns.Types.{Aux, Ord}

final case class Sorted[A](in: Pat[A])(implicit ord: Ord[A]) extends Pattern[A] {
  override private[patterns] def aux: List[Aux] = ord :: Nil

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl(tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val inStream  = in.expand(ctx, tx0)
    private[this] val _valid    = ctx.newVar(false)

    private[this] val sortedIt  = ctx.newVar[Stream[Tx, A]](null)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()    = true
        val xs      = inStream.toList
        sortedIt()  = Stream(xs.sortWith(ord.lt): _*)
      }

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

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

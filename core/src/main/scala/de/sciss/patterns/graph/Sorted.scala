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

import de.sciss.patterns.Types.{Aux, Ord, Top}

final case class Sorted[T <: Top](in: Pat[T])(implicit ord: Ord[T]) extends Pattern[T] {
  override private[patterns] def aux: List[Aux] = ord :: Nil

  def iterator[Tx](implicit ctx: Context[Tx]): Stream[Tx, T#Out[Tx]] = new Stream[Tx, T#Out[Tx]] {
    private[this] val inStream  = in.expand
    private[this] val _valid    = ctx.newVar(false)

    private[this] val sortedIt  = ctx.newVar[Iterator[T#Out[Tx]]](null)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()    = true
        val xs      = inStream.toList
        sortedIt()  = xs.sortWith(ord.lt).iterator
      }

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      sortedIt().hasNext
    }

    def next()(implicit tx: Tx): T#Out[Tx] = {
      validate()
      sortedIt().next()
    }
  }
}

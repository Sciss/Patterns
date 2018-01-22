/*
 *  It.scala
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

import de.sciss.patterns.Types.Top

/** A glue element to make `map` and `flatMap` work. */
final case class It[T <: Top](token: Int) extends Pattern[T] {
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = new StreamImpl

  private final class StreamImpl[Tx](implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    private def outer(implicit tx: Tx) = ctx.getOuterStream[T#Out[Tx]](token)

    def reset()(implicit tx: Tx): Unit       = ()
    def hasNext(implicit tx: Tx): Boolean    = outer.hasNext
    def next ()(implicit tx: Tx): T#Out[Tx]  = outer.next()
  }
}

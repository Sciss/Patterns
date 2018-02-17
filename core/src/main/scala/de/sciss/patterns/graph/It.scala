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

/** A glue element to make `map` and `flatMap` work. */
final case class It[A](token: Int) extends Pattern[A] { pat =>
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = {
    logStream(s"$pat.iterator")
    new StreamImpl[Tx](tx)
  }

  def transform(t: Transform): Pat[A] = this

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val refStream = ctx.mkOuterStream(token)(tx0)

    def reset()(implicit tx: Tx): Unit      = refStream.reset()
    def hasNext(implicit tx: Tx): Boolean   = refStream.hasNext
    def next ()(implicit tx: Tx): A         = refStream.next()
  }
}

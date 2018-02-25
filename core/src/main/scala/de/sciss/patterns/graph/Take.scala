/*
 *  Take.scala
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

import de.sciss.patterns.graph.impl.TruncateStream

final case class Take[A](in: Pat[A], length: Pat[Int]) extends Pattern[A] {

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] =
    new StreamImpl[Tx](tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val inT     = t(in)     .transform(t)
    val lengthT = t(length) .transform(t)
    if (inT.eq(in) && lengthT.eq(length)) this else copy(in = inT, length = lengthT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends TruncateStream[A, Tx](in, length, tx0) {

    protected def truncate(it: Stream[Tx, A], n: Int)(implicit tx: Tx): Stream[Tx, A] =
      it.take(n)
  }
}
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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.impl.TruncateStream

final case class Take[A](in: Pat[A], length: Pat[Int]) extends Pattern[A] {

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT     = t(in)     .transform(t)
    val lengthT = t(length) .transform(t)
    if (inT.eq(in) && lengthT.eq(length)) this else copy(in = inT, length = lengthT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S])
    extends TruncateStream[S, A](in, length, tx0) {

    protected def truncate(it: Stream[S, A], n: Int)(implicit tx: S#Tx): Stream[S, A] =
      it.take(n)
  }
}
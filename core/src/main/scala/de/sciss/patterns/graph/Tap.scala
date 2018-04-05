/*
 *  Tap.scala
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

final case class Tap[A, A1](in: Pat[A], side: Pat[A1]) extends Pattern[A] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT   = t(in)
    val sideT = t(side)
    if (inT.eq(in) && sideT.eq(side)) this else copy(in = inT, side = sideT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, A] {
    private[this] val inStream    = in  .expand(ctx, tx0)
    private[this] val sideStream  = side.expand(ctx, tx0)

    def reset()(implicit tx: S#Tx): Unit = {
      inStream  .reset()
      sideStream.reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
      inStream.hasNext

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      val res = inStream.next()
      if (sideStream.hasNext) sideStream.next()
      res
    }
  }
}
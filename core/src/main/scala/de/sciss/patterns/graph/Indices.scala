/*
 *  Indices.scala
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

case class Indices[A](in: Pat[A]) extends Pattern[Int] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Int] = new StreamImpl(tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Int] = {
    val inT = t(in)
    if (inT eq in) this else copy(in = inT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, Int] {
    private[this] val id        = tx0.newId()
    private[this] val inStream  = in.expand(ctx, tx0)
    private[this] val count     = tx0.newIntVar(id, 0)

    def reset()(implicit tx: S#Tx): Unit = {
      inStream.reset()
      count() = 0
    }

    def hasNext(implicit tx: S#Tx): Boolean =
      inStream.hasNext

    def next()(implicit tx: S#Tx): Int = {
      val res = count()
      inStream.next()
      count() = res + 1
      res
    }
  }
}

/*
 *  Length.scala
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

case class Length[A](in: Pat[A]) extends Pattern[Int] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Int] = new StreamImpl(tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Int] = {
    val inT = t(in)
    if (inT.eq(in)) this else copy(in = inT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, Int] {

    private[this] val id        = tx0.newId()
    private[this] val inStream  = in.expand(ctx, tx0)
    private[this] val _hasNext  = tx0.newBooleanVar(id, true)

    def reset()(implicit tx: S#Tx): Unit = {
      inStream.reset()
      _hasNext() = true
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = _hasNext()

    def next()(implicit ctx: Context[S], tx: S#Tx): Int = {
      if (!hasNext) Stream.exhausted()
      var res = 0
      while (inStream.hasNext) {
        inStream.next()
        res += 1
      }
      _hasNext() = false
      res
    }
  }
}

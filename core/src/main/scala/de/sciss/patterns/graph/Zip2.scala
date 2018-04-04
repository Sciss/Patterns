/*
 *  Zip2.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package graph

import de.sciss.lucre.stm.Base
import de.sciss.patterns

final case class Zip2[A1, A2](a: Pat[A1], b: Pat[A2])
  extends Pattern[(A1, A2)] {

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): patterns.Stream[S, (A1, A2)] =
    new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[(A1, A2)] = {
    val aT = t(a)
    val bT = t(b)
    if (aT.eq(a) && bT.eq(b)) this else copy(a = aT, b = bT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S])
    extends Stream[S, (A1, A2)] {

    private[this] val aStream = a.expand(ctx, tx0)
    private[this] val bStream = b.expand(ctx, tx0)

    def reset()(implicit tx: S#Tx): Unit = {
      aStream.reset()
      bStream.reset()
    }

    def hasNext(implicit tx: S#Tx): Boolean = aStream.hasNext && bStream.hasNext

    def next()(implicit tx: S#Tx): (A1, A2) = (aStream.next(), bStream.next())
  }
}
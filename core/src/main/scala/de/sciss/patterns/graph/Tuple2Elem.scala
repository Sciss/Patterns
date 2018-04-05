/*
 *  Tuple2.scala
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

final case class Tuple2_1[A1, A2](in: Pat[(A1, A2)])
  extends Pattern[A1] {

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): patterns.Stream[S, A1] =
    new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A1] = {
    val inT = t(in)
    if (inT.eq(in)) this else copy(in = inT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S])
    extends Stream[S, A1] {

    private[this] val tupStream = in.expand(ctx, tx0)

    def reset()(implicit tx: S#Tx): Unit = tupStream.reset()

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = tupStream.hasNext
    def next ()(implicit ctx: Context[S], tx: S#Tx): A1      = tupStream.next()._1
  }
}

final case class Tuple2_2[A1, A2](in: Pat[(A1, A2)])
  extends Pattern[A2] {

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): patterns.Stream[S, A2] =
    new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A2] = {
    val inT = t(in)
    if (inT.eq(in)) this else copy(in = inT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S])
    extends Stream[S, A2] {

    private[this] val tupStream = in.expand(ctx, tx0)

    def reset()(implicit tx: S#Tx): Unit = tupStream.reset()

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = tupStream.hasNext
    def next ()(implicit ctx: Context[S], tx: S#Tx): A2      = tupStream.next()._2
  }
}


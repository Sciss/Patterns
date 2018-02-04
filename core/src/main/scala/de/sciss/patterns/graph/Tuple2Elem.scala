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

import de.sciss.patterns
import de.sciss.patterns.Types.Top

final case class Tuple2_1[T1 <: Top, T2 <: Top](tup: Pat.Tuple2[T1, T2])
  extends Pattern[T1] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): patterns.Stream[Tx, T1#Out[Tx]] =
    new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends Stream[Tx, T1#Out[Tx]] {

    private[this] val tupStream = tup.expand(ctx, tx0)

    def reset()(implicit tx: Tx): Unit = ()

    def hasNext(implicit tx: Tx): Boolean = tupStream.hasNext

    def next()(implicit tx: Tx): T1#Out[Tx] = tupStream.next()._1
  }
}

final case class Tuple2_2[T1 <: Top, T2 <: Top](tup: Pat.Tuple2[T1, T2])
  extends Pattern[T2] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): patterns.Stream[Tx, T2#Out[Tx]] =
    new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends Stream[Tx, T2#Out[Tx]] {

    private[this] val tupStream = tup.expand(ctx, tx0)

    def reset()(implicit tx: Tx): Unit = ()

    def hasNext(implicit tx: Tx): Boolean = tupStream.hasNext

    def next()(implicit tx: Tx): T2#Out[Tx] = tupStream.next()._2
  }
}


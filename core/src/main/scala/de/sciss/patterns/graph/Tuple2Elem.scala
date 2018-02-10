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

final case class Tuple2_1[A1, A2](tup: Pat[(A1, A2)])
  extends Pattern[A1] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): patterns.Stream[Tx, A1] =
    new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends Stream[Tx, A1] {

    private[this] val tupStream = tup.expand(ctx, tx0)

    def reset()(implicit tx: Tx): Unit = ()

    def hasNext(implicit tx: Tx): Boolean = tupStream.hasNext

    def next()(implicit tx: Tx): A1 = tupStream.next()._1
  }
}

final case class Tuple2_2[A1, A2](tup: Pat[(A1, A2)])
  extends Pattern[A2] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): patterns.Stream[Tx, A2] =
    new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends Stream[Tx, A2] {

    private[this] val tupStream = tup.expand(ctx, tx0)

    def reset()(implicit tx: Tx): Unit = ()

    def hasNext(implicit tx: Tx): Boolean = tupStream.hasNext

    def next()(implicit tx: Tx): A2 = tupStream.next()._2
  }
}


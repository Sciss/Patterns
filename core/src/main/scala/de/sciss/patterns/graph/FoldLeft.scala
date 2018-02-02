/*
 *  FoldLeft.scala
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

import de.sciss.patterns.Types.Top

final case class FoldLeft[T1 <: Top, T <: Top](outer: Pat[Pat[T1]], z: Pat[T], it1: It[T1], it2: It[T],
                                               inner: Graph[T])
  extends Pattern[T] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] =
    new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    def reset()(implicit tx: Tx): Unit = ???

    def hasNext(implicit tx: Tx): Boolean = ???

    def next()(implicit tx: Tx): T#Out[Tx] = ???
  }
}
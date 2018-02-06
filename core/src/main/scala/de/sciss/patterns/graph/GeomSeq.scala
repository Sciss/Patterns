/*
 *  GeomSeq.scala
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

import de.sciss.patterns.Types.{Aux, Num, Top, Widen}
import de.sciss.patterns.graph.impl.SeriesLikeStreamImpl

final case class GeomSeq[T1 <: Top, T2 <: Top, T <: Top](start: Pat[T1], step: Pat[T2] /* , length: Pat.Int = Int.MaxValue */)
                                                        (implicit protected val widen: Widen[T1, T2, T], num: Num[T])
  extends Pattern[T] {

  override private[patterns] def aux: List[Aux] = widen :: num :: Nil

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends SeriesLikeStreamImpl[T1, T2, T, Tx](start, step /* , length */, tx0) {

    protected def op(a: T#Out[Tx], b: T#Out[Tx]): T#Out[Tx] = num.times(a, b)
  }
}

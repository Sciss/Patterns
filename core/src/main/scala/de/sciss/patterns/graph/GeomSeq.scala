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

import de.sciss.patterns.Types.{Aux, Num, Widen}
import de.sciss.patterns.graph.impl.SeriesLikeStreamImpl

final case class GeomSeq[A1, A2, A](start: Pat[A1], step: Pat[A2] /* , length: Pat.Int = Int.MaxValue */)
                                   (implicit protected val widen: Widen[A1, A2, A], num: Num[A])
  extends Pattern[A] {

  override private[patterns] def aux: List[Aux] = widen :: num :: Nil

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends SeriesLikeStreamImpl[A1, A2, A, Tx](start, step /* , length */, tx0) {

    protected def op(a: A, b: A): A = num.times(a, b)
  }
}

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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.Types.{Aux, Num, Widen2}
import de.sciss.patterns.graph.impl.SeriesLikeStreamImpl

final case class GeomSeq[A1, A2, A](start: Pat[A1], step: Pat[A2] /* , length: Pat.Int = Int.MaxValue */)
                                   (implicit protected val widen: Widen2[A1, A2, A], num: Num[A])
  extends Pattern[A] {

  override private[patterns] def aux: List[Aux] = widen :: num :: Nil

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val startT = t(start)
    val stepT  = t(step )
    if (startT.eq(start) && stepT.eq(step)) this else copy(start = startT, step = stepT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S])
    extends SeriesLikeStreamImpl[S, A1, A2, A](start, step /* , length */, tx0) {

    protected def op(a: A, b: A): A = num.*(a, b)
  }
}

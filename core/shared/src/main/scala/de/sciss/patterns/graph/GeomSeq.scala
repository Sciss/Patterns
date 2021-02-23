/*
 *  GeomSeq.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package graph

import de.sciss.lucre.Adjunct.{Num, Widen2}
import de.sciss.lucre.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.Exec
import de.sciss.patterns.stream.GeomSeqImpl

final case class GeomSeq[A1, A2, A](start: Pat[A1], factor: Pat[A2] /* , length: Pat.Int = Int.MaxValue */)
                                   (implicit val widen: Widen2[A1, A2, A], val num: Num[A])
  extends Pattern[A] with ProductWithAdjuncts {

  override def adjuncts: List[Adjunct] = widen :: num :: Nil

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    GeomSeqImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val startT = t(start)
    val stepT  = t(factor )
    if (startT.eq(start) && stepT.eq(factor)) this else copy(start = startT, factor = stepT)
  }
}

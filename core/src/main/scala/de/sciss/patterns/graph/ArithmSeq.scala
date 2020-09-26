/*
 *  ArithmSeq.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2020 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.patterns.stream.ArithmSeqImpl

/** A pattern that generates an arithmetic series. Corresponds to `Pseries` in SuperCollider. */
final case class ArithmSeq[A1, A2, A](start: Pat[A1], step: Pat[A2])
                                     (implicit val widen: Widen2[A1, A2, A], val num: Num[A])
  extends Pattern[A] with ProductWithAdjuncts {

  override def adjuncts: List[Adjunct] = widen :: num :: Nil

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    ArithmSeqImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val startT = t(start)
    val stepT  = t(step )
    if (startT.eq(start) && stepT.eq(step)) this else copy(start = startT, step = stepT)
  }
}

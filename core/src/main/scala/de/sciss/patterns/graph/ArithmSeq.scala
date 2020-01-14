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

import de.sciss.lucre.adjunct.Adjunct.{Num, Widen2}
import de.sciss.lucre.adjunct.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.stm.Base
import de.sciss.patterns.stream.ArithmSeqImpl

/** A pattern that generates an arithmetic series. Corresponds to `Pseries` in SuperCollider. */
final case class ArithmSeq[A1, A2, A](start: Pat[A1], step: Pat[A2])
                                     (implicit val widen: Widen2[A1, A2, A], val num: Num[A])
  extends Pattern[A] with ProductWithAdjuncts {

  override def adjuncts: List[Adjunct] = widen :: num :: Nil

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    ArithmSeqImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val startT = t(start)
    val stepT  = t(step )
    if (startT.eq(start) && stepT.eq(step)) this else copy(start = startT, step = stepT)
  }
}

/*
 *  Brown.scala
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

import de.sciss.lucre.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.Adjunct.{Num, Widen2}
import de.sciss.lucre.Exec
import de.sciss.patterns.stream.BrownImpl

final case class Brown[A1, A2, A](lo: Pat[A1], hi: Pat[A1], step: Pat[A2])
                                 (implicit val widen: Widen2[A1, A2, A], val num: Num[A])
  extends Pattern[A] with ProductWithAdjuncts {

  override def adjuncts: List[Adjunct] = widen :: num :: Nil

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    BrownImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val loT   = t(lo)
    val hiT   = t(hi)
    val stepT = t(step)
    if (loT.eq(lo) && hiT.eq(hi) && stepT.eq(step)) this else copy(lo = loT, hi = hiT, step = stepT)
  }
}

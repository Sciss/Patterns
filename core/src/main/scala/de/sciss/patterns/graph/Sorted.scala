/*
 *  Sorted.scala
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

import de.sciss.lucre.adjunct.Adjunct.ScalarOrd
import de.sciss.lucre.adjunct.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.stm.Base
import de.sciss.patterns.stream.SortedImpl

final case class Sorted[A](in: Pat[A])(implicit val ord: ScalarOrd[A])
  extends Pattern[A] with ProductWithAdjuncts {

  override def adjuncts: List[Adjunct] = ord :: Nil

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    SortedImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT = t(in)
    if (inT eq in) this else copy(in = inT)
  }
}

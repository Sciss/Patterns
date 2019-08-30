/*
 *  Differentiate.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package graph

import de.sciss.lucre.aux.{Aux, ProductWithAux}
import de.sciss.lucre.aux.Aux.Num
import de.sciss.lucre.stm.Base
import de.sciss.patterns.stream.DifferentiateImpl

final case class Differentiate[A](in: Pat[A])(implicit val num: Num[A])
  extends Pattern[A] with ProductWithAux {

  override def aux: List[Aux] = num :: Nil

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    DifferentiateImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT = t(in)
    if (inT eq in) this else copy(in = inT)
  }
}
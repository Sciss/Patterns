/*
 *  White.scala
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

import de.sciss.lucre.aux.Aux.Num
import de.sciss.lucre.aux.{Aux, ProductWithAux}
import de.sciss.lucre.stm.Base
import de.sciss.patterns.stream.WhiteImpl

final case class White[A](lo: Pat[A], hi: Pat[A])(implicit val num: Num[A])
  extends Pattern[A] with ProductWithAux { pat =>

  override def aux: List[Aux] = num :: Nil

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    WhiteImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val loT = t(lo)
    val hiT = t(hi)
    if (loT.eq(lo) && hiT.eq(hi)) this else copy(lo = loT, hi = hiT)
  }
}
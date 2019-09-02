/*
 *  White.scala
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

import de.sciss.lucre.adjunct.Adjunct.Num
import de.sciss.lucre.adjunct.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.stm.Base
import de.sciss.patterns.stream.WhiteImpl

final case class White[A](lo: Pat[A], hi: Pat[A])(implicit val num: Num[A])
  extends Pattern[A] with ProductWithAdjuncts { pat =>

  override def adjuncts: List[Adjunct] = num :: Nil

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    WhiteImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val loT = t(lo)
    val hiT = t(hi)
    if (loT.eq(lo) && hiT.eq(hi)) this else copy(lo = loT, hi = hiT)
  }
}
/*
 *  White.scala
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

import de.sciss.lucre.Adjunct.Num
import de.sciss.lucre.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.Exec
import de.sciss.patterns.stream.WhiteImpl

final case class White[A](lo: Pat[A], hi: Pat[A])(implicit val num: Num[A])
  extends Pattern[A] with ProductWithAdjuncts { pat =>

  override def adjuncts: List[Adjunct] = num :: Nil

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    WhiteImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val loT = t(lo)
    val hiT = t(hi)
    if (loT.eq(lo) && hiT.eq(hi)) this else copy(lo = loT, hi = hiT)
  }
}
/*
 *  Sorted.scala
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

import de.sciss.lucre.Adjunct.ScalarOrd
import de.sciss.lucre.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.Exec
import de.sciss.patterns.stream.SortedImpl

final case class Sorted[A](in: Pat[A])(implicit val ord: ScalarOrd[A])
  extends Pattern[A] with ProductWithAdjuncts {

  override def adjuncts: List[Adjunct] = ord :: Nil

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    SortedImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val inT = t(in)
    if (inT eq in) this else copy(in = inT)
  }
}

/*
 *  Cat.scala
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

import de.sciss.lucre.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.Adjunct.Widen2
import de.sciss.lucre.Exec
import de.sciss.patterns.stream.CatImpl

final case class Cat[A1, A2, A](a: Pat[A1], b: Pat[A2])
                               (implicit val widen: Widen2[A1, A2, A])
  extends Pattern[A] with ProductWithAdjuncts {

  override def adjuncts: List[Adjunct] = widen :: Nil

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    CatImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val aT = t(a)
    val bT = t(b)
    if (aT.eq(a) && bT.eq(b)) this else copy(a = aT, b = bT)
  }
}
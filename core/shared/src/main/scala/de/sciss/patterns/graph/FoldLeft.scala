/*
 *  FoldLeft.scala
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

import de.sciss.lucre.Exec
import de.sciss.patterns.stream.FoldLeftImpl

final case class FoldLeft[B, A](outer: Pat[Pat[B]], z: Pat[A], itIn: It[B], itCarry: It[A],
                                inner: Pat[A])
  extends Pattern[A] {

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    FoldLeftImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val outerT  = t(outer)
    val zT      = t(z)
    val innerT  = t(inner)
    if (outerT.eq(outer) && zT.eq(z) && innerT.eq(inner)) this else {
      val (itInT    , innerT1) = itIn   .replaceIn(innerT)
      val (itCarryT , innerT2) = itCarry.replaceIn(innerT1)
      copy(outer = outerT, z = zT, itIn = itInT, itCarry = itCarryT, inner = innerT2)
    }
  }
}
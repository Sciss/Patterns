/*
 *  PatMap.scala
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

import de.sciss.lucre.Exec
import de.sciss.patterns.stream.PatMapImpl

final case class PatMap[A1, A] private[patterns](outer: Pat[Pat[A1]], it: It[A1], inner: Pat[A] /* , innerLevel: Int */)
  extends Pattern[Pat[A]] {

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, Pat[A]] =
    PatMapImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[Pat[A]] = {
    val outerT  = t(outer)
    val innerT  = t(inner)
    if (outerT.eq(outer) && innerT.eq(inner)) this else {
      val (itT, innerT1) = it.replaceIn(innerT)
      copy(outer = outerT, it = itT, inner = innerT1)
    }
  }
}

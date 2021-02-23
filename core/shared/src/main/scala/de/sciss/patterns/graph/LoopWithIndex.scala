/*
 *  FlatTabulate.scala
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
import de.sciss.patterns.stream.LoopWithIndexImpl

final case class LoopWithIndex[A] private[patterns](n: Pat[Int], it: It[Int], inner: Pat[A] /* , innerLevel: Int */)
  extends Pattern[A] {

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    LoopWithIndexImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val nT      = t(n)
    val innerT  = t(inner)
    if (nT.eq(n) && innerT.eq(inner)) this else copy(n = nT, inner = innerT)
  }
}

/*
 *  Zip2.scala
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
import de.sciss.patterns.stream.Zip2Impl

final case class Zip2[A1, A2](a: Pat[A1], b: Pat[A2])
  extends Pattern[(A1, A2)] {

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, (A1, A2)] =
    Zip2Impl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[(A1, A2)] = {
    val aT = t(a)
    val bT = t(b)
    if (aT.eq(a) && bT.eq(b)) this else copy(a = aT, b = bT)
  }
}
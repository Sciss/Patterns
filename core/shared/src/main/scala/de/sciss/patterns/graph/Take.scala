/*
 *  Take.scala
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
import de.sciss.patterns.stream.TakeImpl

final case class Take[A](in: Pat[A], length: Pat[Int]) extends Pattern[A] {

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    TakeImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val inT     = t(in)
    val lengthT = t(length)
    if (inT.eq(in) && lengthT.eq(length)) this else copy(in = inT, length = lengthT)
  }
}
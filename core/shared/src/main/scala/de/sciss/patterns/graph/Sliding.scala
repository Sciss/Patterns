/*
 *  Sliding.scala
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

package de.sciss.patterns.graph

import de.sciss.lucre.Exec
import de.sciss.patterns.stream.SlidingImpl
import de.sciss.patterns.{Context, Stream, Transform}

final case class Sliding[A](in: Pat[A], size: Pat[Int], step: Pat[Int]) extends Pattern[Pat[A]] { pat =>

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, Pat[A]] =
    SlidingImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[Pat[A]] = {
    val inT   = t(in)
    val sizeT = t(size)
    val stepT = t(step)
    if (inT.eq(in) && sizeT.eq(size) && stepT.eq(step)) this else copy(in = inT, size = sizeT, step = stepT)
  }
}
/*
 *  Sliding.scala
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

package de.sciss.patterns.graph

import de.sciss.lucre.stm.Base
import de.sciss.patterns.stream.SlidingImpl
import de.sciss.patterns.{Context, Pat, Pattern, Stream, Transform}

final case class Sliding[A](in: Pat[A], size: Pat[Int], step: Pat[Int]) extends Pattern[Pat[A]] { pat =>

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] =
    SlidingImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Pat[A]] = {
    val inT   = t(in)
    val sizeT = t(size)
    val stepT = t(step)
    if (inT.eq(in) && sizeT.eq(size) && stepT.eq(step)) this else copy(in = inT, size = sizeT, step = stepT)
  }
}
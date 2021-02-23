/*
 *  Tuple2.scala
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
import de.sciss.patterns.stream.{Tuple2_1Impl, Tuple2_2Impl}

final case class Tuple2_1[A, A1](in: Pat[(A, A1)])
  extends Pattern[A] {

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    Tuple2_1Impl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val inT = t(in)
    if (inT.eq(in)) this else copy(in = inT)
  }
}

final case class Tuple2_2[A1, A](in: Pat[(A1, A)])
  extends Pattern[A] {

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    Tuple2_2Impl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val inT = t(in)
    if (inT.eq(in)) this else copy(in = inT)
  }
}


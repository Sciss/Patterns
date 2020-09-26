/*
 *  Bubble.scala
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
import de.sciss.patterns.stream.BubbleImpl

final case class Bubble[A](in: Pat[A]) extends Pattern[Pat[A]] { pat =>
  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, Pat[A]] =
    BubbleImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[Pat[A]] = {
    val inT = t(in)
    if (inT.eq(in)) this else copy(in = inT)
  }
}

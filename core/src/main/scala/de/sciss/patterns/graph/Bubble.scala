/*
 *  Bubble.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package graph

import de.sciss.lucre.stm.Base
import de.sciss.patterns.stream.BubbleImpl

final case class Bubble[A](in: Pat[A]) extends Pattern[Pat[A]] { pat =>
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] =
    BubbleImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Pat[A]] = {
    val inT = t(in)
    if (inT.eq(in)) this else copy(in = inT)
  }
}
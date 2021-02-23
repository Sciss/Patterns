/*
 *  It.scala
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
import de.sciss.patterns.stream.ItImpl

/** A glue element to make `map` and `flatMap` work. */
final case class It[A](token: Int) extends Pattern[A] { pat =>
  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    ItImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = this

  def replaceIn[T <: Exec[T], B](inner: Pat[B])(implicit ctx: Context[T], tx: T): (It[A], Pat[B]) = {
    val itT = ctx.allocToken[A]()
    val t = new Transform {
      def applyOne[X](in: Pat[X]): Pat[X] = in match {
        case `pat`  => itT.asInstanceOf[Pat[X]]
        case other  => other
      }
    }
    val innerT = t(inner)
    (itT, innerT)
  }
}

/*
 *  It.scala
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

/** A glue element to make `map` and `flatMap` work. */
final case class It[A](token: Int) extends Pattern[A] { pat =>
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    impl.ItImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = this

  def replaceIn[S <: Base[S], B](inner: Pat[B])(implicit ctx: Context[S], tx: S#Tx): (It[A], Pat[B]) = {
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

/*
 *  Constant.scala
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
import de.sciss.patterns.stream.ConstantImpl

final case class Constant[A](value: A) extends Pat[A] {
  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    ConstantImpl(value)

  // $COVERAGE-OFF$
  override def toString: String = value.toString
  // $COVERAGE-ON$

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = this
}
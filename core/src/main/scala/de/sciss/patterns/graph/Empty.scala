/*
 *  Empty.scala
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
import de.sciss.patterns.stream.EmptyImpl

final case class Empty() extends Pat[Nothing] { pat =>
  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, Nothing] =
    EmptyImpl()

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[Nothing] = this
}
/*
 *  Transform.scala
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

import de.sciss.lucre.Exec

trait Transform {
  final def apply[T <: Exec[T], A](in: Pat[A])(implicit ctx: Context[T], tx: T): Pat[A] =
    applyOne(in).transform(this)

  protected def applyOne[A](in: Pat[A]): Pat[A]
}

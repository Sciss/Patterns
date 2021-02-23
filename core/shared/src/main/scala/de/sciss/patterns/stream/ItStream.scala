/*
 *  ItStream.scala
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
package stream

import de.sciss.lucre.Exec

trait ItStream[T <: Exec[T], +A] extends Stream[T, A] {
  def token: Int
}

trait AdvanceItStream[T <: Exec[T], +A] extends ItStream[T, A] {
  def resetOuter()(implicit tx: T): Unit

  def advance()(implicit ctx: Context[T], tx: T): Unit
}

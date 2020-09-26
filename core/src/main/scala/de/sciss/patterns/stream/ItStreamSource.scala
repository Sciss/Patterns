/*
 *  ItStreamSource.scala
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
package stream

import de.sciss.lucre.Exec

trait ItStreamSource[T <: Exec[T], A] {
  def token: Int

  def mkItStream()(implicit ctx: Context[T], tx: T): ItStream[T, A]

  def registerItStream(stream: ItStream[T, A])(implicit tx: T): Unit
}

/*
 *  StreamFactory.scala
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
import de.sciss.serial.DataInput

trait StreamFactory {
  def typeId: Int

  def readIdentified[T <: Exec[T]](in: DataInput)(implicit ctx: Context[T], tx: T): Stream[T, Any]
}

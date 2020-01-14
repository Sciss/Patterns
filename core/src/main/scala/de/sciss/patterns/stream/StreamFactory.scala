/*
 *  StreamFactory.scala
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

import de.sciss.lucre.stm.Base
import de.sciss.serial.DataInput

trait StreamFactory {
  def typeId: Int

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)(implicit ctx: Context[S], tx: S#Tx): Stream[S, Any]
}

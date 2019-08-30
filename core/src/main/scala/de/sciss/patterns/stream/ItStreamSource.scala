/*
 *  ItStreamSource.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
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

trait ItStreamSource[S <: Base[S], A] {
  def token: Int

  def mkItStream()(implicit ctx: Context[S], tx: S#Tx): ItStream[S, A]

  def registerItStream(stream: ItStream[S, A])(implicit tx: S#Tx): Unit
}

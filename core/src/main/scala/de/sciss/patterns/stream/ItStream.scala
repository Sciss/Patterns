/*
 *  ItStream.scala
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

trait ItStream[S <: Base[S], +A] extends Stream[S, A] {
  def token: Int
}

trait AdvanceItStream[S <: Base[S], +A] extends ItStream[S, A] {
  def resetOuter()(implicit tx: S#Tx): Unit

  def advance()(implicit ctx: Context[S], tx: S#Tx): Unit
}

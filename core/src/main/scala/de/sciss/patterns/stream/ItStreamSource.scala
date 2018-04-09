/*
 *  ItStreamSource.scala
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
package stream

import de.sciss.lucre.stm.Base

trait ItStreamSource[S <: Base[S], A] {
  def mkItStream()(implicit tx: S#Tx): Stream[S, A]

  def pingFromIt(token: Int, stream: Stream[S, A])(implicit tx: S#Tx): Unit
}

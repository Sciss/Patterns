/*
 *  FoldLeftCarryStream.scala
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
package graph
package impl

final class FoldLeftCarryStream[Tx, A](buf: FoldLeftCarryBuffer[Tx, A])
  extends Stream[Tx, A] { id =>

//  def resetOuter()(implicit tx: Tx): Unit =
//    _valid() = false

  def reset()(implicit tx: Tx): Unit = {
    println("FoldLeftInStream. TODO: reset")
    ()
  }

  def hasNext(implicit tx: Tx): Boolean =
    buf.hasNext(id)

  def next()(implicit tx: Tx): A =
    buf.next(id)
}

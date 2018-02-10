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

import de.sciss.patterns.Types.Top

final class FoldLeftCarryStream[Tx, T <: Top](buf: FoldLeftCarryBuffer[Tx, T])
  extends Stream[Tx, T#Out[Tx]] { id =>

  type A = T#Out[Tx]

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

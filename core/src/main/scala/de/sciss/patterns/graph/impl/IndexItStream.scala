/*
 *  IndexItStream.scala
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

import de.sciss.lucre.stm.Base

final class IndexItStream[S <: Base[S]](iteration: S#Var[Int], tx0: S#Tx)
  extends Stream[S, Int] {

  private[this] val id        = tx0.newId()
  private[this] val _hasNext  = tx0.newBooleanVar(id, true)

  def reset()(implicit tx: S#Tx): Unit =
    _hasNext() = true

  def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = _hasNext()

  def next()(implicit ctx: Context[S], tx: S#Tx): Int = {
    if (!hasNext) Stream.exhausted()
    _hasNext() = false
    iteration()
  }
}

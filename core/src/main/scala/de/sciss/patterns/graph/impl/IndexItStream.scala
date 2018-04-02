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

final class IndexItStream[Tx](iteration: Context.Var[Tx, Int], tx0: Tx)(implicit ctx: Context[Tx])
  extends Stream[Tx, Int] {

  private[this] val id        = ctx.newID()(tx0)
  private[this] val _hasNext  = ctx.newBooleanVar(id, true)(tx0)

  def reset()(implicit tx: Tx): Unit =
    _hasNext() = true

  def hasNext(implicit tx: Tx): Boolean = _hasNext()

  def next()(implicit tx: Tx): Int = {
    if (!hasNext) Stream.exhausted()
    _hasNext() = false
    iteration()
  }
}

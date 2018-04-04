/*
 *  TruncateStream.scala
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

abstract class TruncateStream[S <: Base[S], A](in: Pat[A], length: Pat[Int], tx0: S#Tx)(implicit ctx: Context[S])
  extends Stream[S, A] {

  private[this] val id        = tx0.newId()
  private[this] val lenStream = length.expand(ctx, tx0)
  private[this] val inStream  = in    .expand(ctx, tx0)

  private[this] val peer      = ??? : S#Var[Stream[S, A]] // ctx.newVar[Stream[S, A]](null)(tx0)
  private[this] val _hasNext  = tx0.newBooleanVar(id, false)
  private[this] val _valid    = tx0.newBooleanVar(id, false)

  protected def truncate(it: Stream[S, A], n: Int)(implicit tx: S#Tx): Stream[S, A]

  def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
    _valid() = false
    lenStream .reset()
    inStream  .reset()
//      if (_hasPeer()) peer().reset()
  }

  private def validate()(implicit tx: S#Tx): Unit = if (!_valid()) {
    _valid() = true
    val lhn = lenStream.hasNext
    if (lhn) {
      val lenVal = lenStream.next()
      peer()      = truncate(inStream, lenVal)
//      _hasPeer()  = true
      _hasNext()  = peer().hasNext
    } else {
//      _hasPeer()  = false
      _hasNext()  = false
    }
  }

  def hasNext(implicit tx: S#Tx): Boolean = {
    validate()
    _hasNext()
  }

  def next()(implicit tx: S#Tx): A = {
    if (!hasNext) Stream.exhausted()
    val res = peer().next()
    _hasNext() = peer().hasNext
    res
  }
}

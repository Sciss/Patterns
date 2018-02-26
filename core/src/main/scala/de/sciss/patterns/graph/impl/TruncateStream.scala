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

abstract class TruncateStream[A, Tx](in: Pat[A], length: Pat[Int], tx0: Tx)(implicit ctx: Context[Tx])
  extends Stream[Tx, A] {

  private[this] val lenStream = length.expand(ctx, tx0)
  private[this] val inStream  = in    .expand(ctx, tx0)

  private[this] val peer      = ctx.newVar[Stream[Tx, A]](null)
//  private[this] val _hasPeer  = ctx.newVar(false)
  private[this] val _hasNext  = ctx.newVar(false)
  private[this] val _valid    = ctx.newVar(false)

  protected def truncate(it: Stream[Tx, A], n: Int)(implicit tx: Tx): Stream[Tx, A]

  def reset()(implicit tx: Tx): Unit = if (_valid()) {
    _valid() = false
    lenStream .reset()
    inStream  .reset()
//      if (_hasPeer()) peer().reset()
  }

  private def validate()(implicit tx: Tx): Unit = if (!_valid()) {
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

  def hasNext(implicit tx: Tx): Boolean = {
    validate()
    _hasNext()
  }

  def next()(implicit tx: Tx): A = {
    if (!hasNext) Stream.exhausted()
    val res = peer().next()
    _hasNext() = peer().hasNext
    res
  }
}
